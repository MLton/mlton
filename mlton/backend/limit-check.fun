      (* ------------------------------------------------- *)
      (*                 insertLimitChecks                 *)
      (* ------------------------------------------------- *)
	 
      fun insertLimitChecks (self as T {chunkLabel, blocks,
					entries, gcReturns, ...},
			     frames): unit =
	 let
	    val _ =
	       Control.diagnostics
	       (fn display =>
		let
		   open Layout
		   val _ = display (seq [str "limit checks for ",
					 ChunkLabel.layout chunkLabel])
		in
		   ()
		end)
	    val returns: Label.t list ref = ref []

	    fun newFrame (GCInfo.T {frameSize, live, return, ...}) =
	       let val l = Label.newNoname ()
		   val liveOffsets
		     = List.fold
		       (live,
			[],
			fn (oper, liveOffsets)
			 => case Operand.deStackOffset oper
			      of SOME {offset, ty}
			       => (case Type.dest ty
				     of Type.Pointer => offset::liveOffsets
				      | _ => liveOffsets)
			       | NONE => liveOffsets)
	       in List.push (returns, l)
		  ; Frames.add (frames, {return = l,
					 chunkLabel = chunkLabel,
					 size = frameSize,
					 offsets = liveOffsets})
		  ; return := SOME l
	       end
	    val {get = labelBlock, set = setLabelBlock, destroy} =
	       Property.destGetSetOnce
	       (Label.plist, Property.initRaise ("block", Label.layout))
	    open Block Transfer
	    val _ = List.foreach (!blocks, fn b as T {label, ...} =>
				  setLabelBlock (label, b))
	    fun memo (l: Label.t): int =
	       let
		  val T {bytesNeeded, label, statements, transfer, ...} =
		     labelBlock l
	       in
		  case !bytesNeeded of
		     SOME n => n
		   | NONE =>
			let
			   val _ = bytesNeeded := SOME 0
			   val goto = memo
			   val rest =
			      case transfer of
			         Arith {overflow, success, ...} =>
				    Int.max (goto overflow, goto success)
			       | NearJump {label, ...} => goto label
			       | SwitchIP {int, pointer, ...} =>
				    Int.max (goto int, goto pointer)
			       | Switch {cases, default, ...} =>
				    Cases.fold
				    (cases, (case default of
						NONE => 0
					      | SOME l => goto l),
				     fn (j, rest) => Int.max (goto j, rest))
			       | _ => 0
			   fun allocateArray
			      ({user = {gcInfo, numElts, numPointers,
					numBytesNonPointers, ...},
				limitCheck}: Statement.allocateArray,
			       bytesAllocated: int): int =
			      let
				 val bytesPerElt =
				    if numPointers = 0
				       then numBytesNonPointers
				    else if numBytesNonPointers = 0
					    then numPointers * pointerSize
					 else Error.unimplemented "tricky arrays"
				 val bytesAllocated =
				    bytesAllocated
				    (* space for array header *)
				    + arrayHeaderSize
				    (* space for forwarding pointer for zero
				     * length arrays *)
				    + pointerSize
				 fun here () =
				    let
				       val _ = newFrame gcInfo
				       val lc = {bytesPerElt = bytesPerElt,
						 bytesAllocated = bytesAllocated}
				       val _ = limitCheck := SOME lc
				    in
				       0
				    end
				 (* maxArrayLimitCheck is arbitrary -- it's just
				  * there to ensure that really huge array
				  * allocations don't get moved too early.
				  *)
				 val maxArrayLimitCheck = 10000
			      in
				 case numElts of
				    Operand.Int numElts =>
				       if numElts <= maxArrayLimitCheck
					  then
					     (bytesAllocated +
					      Type.align (Type.pointer,
							  numElts * bytesPerElt))
					     handle Exn.Overflow => here ()
				       else here ()
				  | _ => here ()
			      end
			   val bytesAllocated =
			      Array.foldr
			      (statements, rest,
			       fn (statement, bytesAllocated) =>
			       let datatype z = datatype Statement.t
			       in case statement of
				  AllocateArray r =>
				     allocateArray (r, bytesAllocated)
				| Allocate {size, ...} =>
				     objectHeaderSize + size + bytesAllocated
				| Assign {pinfo, ...} =>
				     (case pinfo
					 of PrimInfo.Runtime gcInfo =>
					    newFrame gcInfo
				       | _ => ()
					    ; bytesAllocated)
				| LimitCheck lc =>
				     LimitCheck.set
				     (lc, bytesAllocated, newFrame)
				| _ => bytesAllocated
			       end)
			in bytesNeeded := SOME bytesAllocated
			   ; bytesAllocated
			end
	       end
	    val _ =
	       List.foreach (!entries, fn l => (memo l; ()))
	       handle Exn.Overflow => Error.bug "limit check insertion overflow"
	    val _ = gcReturns := SOME (!returns)
	    val _ = destroy ()
	    val _ =
	       Control.diagnostics
	       (fn display =>
		let
		   open Layout
		   val _ = display (seq [str "limit checks for ",
					 ChunkLabel.layout chunkLabel])
		   val _ =
		      List.foreach
		      (!blocks, fn Block.T {bytesNeeded, label, ...} =>
		       display
		       (seq [Label.layout label, str " ",
			     Option.layout Int.layout (!bytesNeeded)]))
		in
		   ()
		end)
	 in
	    ()
	 end
