
(*
 * Some of this doesn't make sense if we track the liveness of the GCHold class.
 * Need to update the enque'' of returns, handlers of NonTail and Runtime
 * so they reflect what happens at these transfers; (i.e., stackTop and frontier 
 * are defed on return from NonTail).
 *)

functor x86LiveTransfers(S: X86_LIVE_TRANSFERS_STRUCTS) : X86_LIVE_TRANSFERS =
struct
  open S
  open x86
  structure LiveSet = x86Liveness.LiveSet
  structure LiveInfo = x86Liveness.LiveInfo
  open x86JumpInfo

  fun take (l, n) 
    = let
	val rec take'
	  = fn ([], _, ac) => List.rev ac
	     | (_, 0 : Int.t, ac) => List.rev ac
	     | (h::t, i, ac) => take' (t, i - 1, h::ac)
      in
	take' (l, n, [])
      end

  val track = x86Liveness.track

  val tracer = x86.tracer
  val tracerTop = x86.tracerTop

  fun temp_uses_defs {uses : Operand.t list,
		      defs : Operand.t list}
    = let
	val baseUses
	  = List.fold
	    (uses,
	     [],
	     fn (operand, baseUses)
	      => case Operand.deMemloc operand
		   of SOME memloc => if x86Liveness.track memloc
		                        andalso 
					not (List.contains(baseUses,
							   memloc,
							   MemLoc.eq))
				       then memloc::baseUses
				       else baseUses
		    | NONE => baseUses)
	    
	val tempUses
	  = let
	      fun doit (operands, tempUses)
		= List.fold
		  (operands,
		   tempUses,
		   fn (operand, tempUses)
		    => case Operand.deMemloc operand
			 of SOME memloc
			  => List.fold(MemLoc.utilized memloc,
				       tempUses,
				       fn (memloc, tempUses)
				        => if x86Liveness.track memloc
				              andalso
					      not (List.contains
						   (tempUses,
						    memloc,
						    MemLoc.eq))
					     then memloc::tempUses
					     else tempUses)
			| NONE => tempUses)
	    in
	      doit(defs, 
	      doit(uses, 
		   baseUses))
	    end

	val baseDefs
	  = List.fold
	    (defs,
	     [],
	     fn (operand, baseDefs)
	      => case Operand.deMemloc operand
		   of SOME memloc => if x86Liveness.track memloc
		                        andalso 
					not (List.contains(baseDefs,
							   memloc,
							   MemLoc.eq))
				       then memloc::baseDefs
				       else baseDefs
		    | NONE => baseDefs)
	val tempDefs = baseDefs
      in
	{uses = tempUses,
	 defs = tempDefs}
      end

  datatype t = T of {get: Label.t -> 
                          ((MemLoc.t * Register.t * bool) list *
			   (MemLoc.t * bool) list) option,
		     set: Label.t * 
                          ((MemLoc.t * Register.t * bool) list *
			   (MemLoc.t * bool) list) option -> 
                          unit}

  local

  in
    structure I' = Int
    structure I =
      struct
	datatype t = NegInfinity
	           | Finite of I'.t
	           | PosInfinity
	val zero = Finite (I'.zero)
	val one = Finite (I'.one)
	val two = Finite (I'.two)

	fun NegInfinity < NegInfinity = false
	  | NegInfinity < _ = true
	  | (Finite x) < NegInfinity = false
	  | (Finite x) < (Finite y) = I'.<(x,y)
	  | (Finite x) < PosInfinity = true
	  | PosInfinity < _ = false

	fun NegInfinity + PosInfinity = zero
	  | NegInfinity + _ = NegInfinity
	  | (Finite x) + NegInfinity = NegInfinity
	  | (Finite x) + (Finite y) = Finite (I'.+(x,y))
	  | (Finite x) + PosInfinity = PosInfinity
	  | PosInfinity + NegInfinity = zero
	  | PosInfinity + _ = PosInfinity

	fun min (NegInfinity, y) = NegInfinity
	  | min (x, NegInfinity) = NegInfinity
	  | min (PosInfinity, y) = y
	  | min (x, PosInfinity) = x
	  | min (Finite x, Finite y) = Finite (I'.min(x, y))
      end
  end

  fun computeLiveTransfers {chunk as Chunk.T {blocks,...},
			    transferRegs : Register.t list,
			    transferFltRegs : Int.t,
			    liveInfo : x86Liveness.LiveInfo.t,
			    jumpInfo : x86JumpInfo.t}
    = let
	val cutoff = !Native.cutoff

	val info
	  as {get = getInfo : 
	      Label.t -> {block: Block.t,
			  live: (MemLoc.t * 
				 bool * 
				 I.t option ref) list option ref},
	      set = setInfo}
	  = Property.getSetOnce
	    (Label.plist,
	     Property.initRaise ("x86LiveTransfers:getInfo", Label.layout))

	val funcs
	  = List.fold
	    (blocks,
	     [],
	     fn (block as Block.T {entry, ...}, funcs)
	      => let
		   val label = Entry.label entry
		 in 
		   (setInfo(label,
			    {block = block,
			     live = ref NONE});
		    case entry
		      of Entry.Func {label, live} 
		       => label::funcs
		       | _ => funcs)
		 end)
	    
	local
	  val queue = ref (Queue.empty ())
	in
	  fun enque x = queue := Queue.enque(!queue, x)
	  fun deque () = case Queue.deque(!queue)
			   of NONE => NONE
			    | SOME(x, queue') => (queue := queue';
						  SOME x)
	end

	fun doit {label, defed}
	  = let
	      val live = LiveInfo.getLive(liveInfo, label)
	      val defed = MemLocSet.subset
		          (defed,
			   fn memloc
			    => MemLocSet.contains(live, memloc))

	      val {block, live} = getInfo label
	      val Block.T {entry, statements, transfer, ...} = block
	      val (live', changed)
		= case !live
		    of NONE => let
				 val live' = LiveInfo.getLive(liveInfo, label)
				 val live' = List.map
				             (LiveSet.toList live',
					      fn memloc => (memloc, true, ref NONE))
			       in
				 (live', true)
			       end
		     | SOME live' => (live', false)
	      val (live',changed)
		= List.fold
		  (live',
		   ([],changed),
		   fn ((memloc,sync,weight),(live',changed))
		    => let
			 val defed' = MemLocSet.contains(defed, memloc)
			 val sync' = sync andalso (not defed')
		       in
			 ((memloc, sync', weight)::live',
			  changed orelse (sync <> sync'))
		       end)
	    in
	      if changed
		then let
		       val _ = live := SOME live'
		       fun doit' (defed, defs)
			 = List.fold
			   (defs,
			    defed,
			    fn (def,defed)
			     => case Operand.deMemloc def
				  of SOME def => if track def
						   then MemLocSet.add(defed, def)
						   else defed
				   | NONE => defed)

		       val {defs, ...} = Entry.uses_defs_kills entry
		       val defed = doit' (defed, defs)

		       val defed
			 = List.fold
			   (statements,
			    defed,
			    fn (asm,defed)
			     => let
				  val {defs, ...} = Assembly.uses_defs_kills asm
				in
				  doit' (defed, defs)
				end)

		       val {defs, ...} = Transfer.uses_defs_kills transfer
		       val defed = doit' (defed, defs)

		       fun enque' label = enque {label = label, 
						 defed = defed}
		       fun enque'' label = enque {label = label, 
						  defed = MemLocSet.empty}
		       datatype z = datatype Transfer.t
		     in
		       case transfer
			 of Goto {target, ...}
			  => (enque' target)
			  | Iff {truee, falsee, ...}
			  => (enque' truee;
			      enque' falsee)
			  | Switch {cases, default, ...}
			  => (Transfer.Cases.foreach(cases, enque');
			      enque' default)
			  | Tail {...}
			  => ()
			  | NonTail {return, handler, ...}
			  => (enque'' return;
			      case handler 
				of SOME handler => enque'' handler
				 | NONE => ())
			  | Return {...}
			  => ()
			  | Raise {...}
			  => ()
			  | Runtime {return, ...}
			  => (enque'' return)
			  | CCall {dst, return, ...}
			  => (enque' return)
		     end
		else ()
	    end
	  
	val _ = List.foreach
	        (funcs,
		 fn label => enque {label = label, 
				    defed = MemLocSet.empty})
	fun loop ()
	  = (case deque ()
	       of NONE => ()
	        | SOME {label, defed}
		=> (doit {label = label, defed = defed};
		    loop ()))
	val _ = loop ()

	(* update weights *)
	fun get_distanceF' {temp : MemLoc.t,
			    label : Label.t}
	  = let
	      val 
		{block, live, ...} = getInfo label
	    in
	      case List.peek(valOf (!live),
			     fn (temp',_,_) => MemLoc.eq(temp, temp'))
		of SOME (_,_,temp_distanceF as (ref (SOME distance))) => distance
		 | SOME (_,_,temp_distanceF as (ref NONE)) 
		 => let
		      val _ = temp_distanceF := SOME I.PosInfinity
			
		      val Block.T {statements, transfer, ...} = block

		      datatype t = Position of I.t | Length of I'.t
		      fun posF (assembly,n as I.Finite n')
			= if n < cutoff
			    then posF'(assembly,n)
			    else Position I.PosInfinity
		      and posF' ([],n) 
			= let
			    val live = Transfer.live transfer
			    val {uses,defs,...} 
			      = Transfer.uses_defs_kills transfer
			    val {uses,defs} 
			      = temp_uses_defs {uses = uses,
						defs = defs}
			  in
			    if List.contains(uses,
					     temp,
					     MemLoc.eq)
			      then Position (I.Finite n)
			      else if List.contains(live,
						    temp,
						    MemLoc.eq)
				     then Position I.PosInfinity
				     else Length (I'.+(n, I'.one))
			  end
			| posF' (asm::assembly,n)
			= let
			    val {uses,defs,...} 
			      = Assembly.uses_defs_kills asm
			    val {uses,defs} 
			      = temp_uses_defs {uses = uses,
						defs = defs}
			  in
			    if List.contains(uses,
					     temp,
					     MemLoc.eq)
			      then Position (I.Finite n)
			      else posF (assembly, I'.+(n, I'.one))
			  end
			
		      val distance
			= case posF(statements, I'.one)
			    of Position n => n
			     | Length n
			     => let
				  val n = I.Finite n
				in 
				  List.fold
				  (Transfer.nearTargets transfer,
				   I.PosInfinity,
				   fn (label, min)
				    => if LiveSet.contains
				          (LiveInfo.getLive(liveInfo, label),
					   temp)
					 then let
						val n'
						  = get_distanceF'
						    {temp = temp,
						     label = label}
					      in 
						I.min(min, I.+(n, n'))
					      end
					 else min)
				end
		    in
		      temp_distanceF := SOME distance;
		      distance
		    end
		 | _ => Error.bug "computeLiveTransfers::get_distanceF'"
	    end
	  
	fun get_distanceF {temp : MemLoc.t,
			   label : Label.t}
	  = let
	      val distance
		= get_distanceF' {temp = temp,
				  label = label}
	    in
	      distance
	    end

	val liveTransferInfo
	 as {get = getLiveTransfers : 
	     Label.t -> ((MemLoc.t * Register.t * bool) list *
			 (MemLoc.t * bool) list) option,
	     set = setLiveTransfers}
	   = Property.getSet
	     (Label.plist, Property.initConst NONE)

	local
	  val queue = ref (Queue.empty ())
	in
	  fun enque x = queue := Queue.enque(!queue, x)
	  fun deque () = case Queue.deque(!queue)
			   of NONE => NONE
			    | SOME(x, queue') => (queue := queue';
						  SOME x)
	end
      
	fun doit {label, hints}
	  = (case getLiveTransfers label
	       of SOME _ => ()
		| NONE 
		=> let
		     val {block, live} = getInfo label
		     val Block.T {transfer, ...} = block

		     val (regHints, fltregHints) = hints

		     val live
		       = List.keepAllMap
		         (valOf (!live),
			  fn (temp,sync,_)
			   => let
				val n = get_distanceF {temp = temp,
						       label = label}
			      in
				case n
				  of I.NegInfinity
				   => Error.bug "computeLiveTransfers::NegInfinity"
				   | I.Finite n'
				   => if n' < cutoff
					then SOME (temp, sync, n')
					else NONE
				   | I.PosInfinity
				   => NONE
			      end)

		     val live
		       = List.insertionSort
		         (live,
			  fn ((_,_,n1),(_,_,n2))
			   => I'.<(n1, n2))

		     val {yes = liveRegs, no = liveFltRegs}
		       = List.partition
		         (live,
			  fn (memloc,_,_) 
			   => Size.class (MemLoc.size memloc) = Size.INT)

		     val liveRegs
		       = List.map
		         (liveRegs,
			  fn (memloc,sync,weight)
			   => case List.peek
			           (regHints,
				    fn (memloc',register',weight')
				     => MemLoc.eq(memloc,memloc'))
				of SOME (memloc',register',weight')
				 => (memloc,sync,weight,SOME register')
				 | NONE 
				 => (memloc,sync,weight,NONE))		  

		     val rec doitRegs
		       = fn ([],_,liveTransfers) => liveTransfers
		          | (_,[],liveTransfers) => liveTransfers
		          | (transferRegs,
			     (memloc,sync,weight,register)::live,
			     liveTransfers)
		          => let
			       fun finish register
				 = let
				     val transferRegs
				       = List.removeAll
				         (transferRegs,
					  fn register'
					   => Register.coincide(register, 
								register'))
				   in
				     doitRegs (transferRegs,
					       live,
					       (memloc,register,sync)::liveTransfers)
				   end

			       fun default ()
				 = let
				     val size = MemLoc.size memloc
				     val transferRegs'
				       = List.keepAllMap
				         (transferRegs,
					  fn register
					   => if Size.eq
					         (size,
						  Register.size register)
						then SOME
						     (register,
						      List.index
						      (live,
						       fn (_,_,_,SOME register')
						        => Register.eq
						           (register,
							    register')
							| (_,_,_,NONE) => false))
						else NONE)
				     val transferRegs'
				       = List.insertionSort
				         (transferRegs',
					  fn ((_,SOME index1),(_,SOME index2))
					   => Int.>(index1, index2)
					   | ((_, NONE),_)
					   => true
					   | (_, (_, NONE))
					   => false)
				   in
				     case transferRegs'
				       of nil
					=> doitRegs (transferRegs,
						     live,
						     liveTransfers)
					| (register,_)::_
					=> finish register
				   end
			     in
			       case register
				 of SOME register
				  => if List.contains(transferRegs,
						      register,
						      Register.eq)
				       then finish register
				       else default ()
				  | NONE => default ()
			     end

		     val liveRegsTransfers = doitRegs(transferRegs, liveRegs, [])


		     val liveFltRegs
		       = take
		         (liveFltRegs,
			  transferFltRegs)
		     val liveFltRegsTransfers
		       = List.map
		         (liveFltRegs,
			  fn (memloc,sync,_) => (memloc, sync))
			  

		     val liveTransfers = (liveRegsTransfers, liveFltRegsTransfers)

		     val _ = setLiveTransfers(label, SOME liveTransfers)
		     fun enque' label = enque {label = label, 
					       hints = liveTransfers}
		     fun enque'' label = enque {label = label, 
						hints = ([],[])}
		     datatype z = datatype Transfer.t
		   in
		       case transfer
			 of Goto {target, ...}
			  => (enque' target)
			  | Iff {truee, falsee, ...}
			  => (enque' truee;
			      enque' falsee)
			  | Switch {cases, default, ...}
			  => (Transfer.Cases.foreach(cases, enque');
			      enque' default)
			  | Tail {...}
			  => ()
			  | NonTail {return, handler, ...}
			  => (enque'' return;
			      case handler 
				of SOME handler => enque'' handler
				 | NONE => ())
			  | Return {...}
			  => ()
			  | Raise {...}
			  => ()
			  | Runtime {return, ...}
			  => (enque'' return)
			  | CCall {dst, return, ...}
			  => (enque'' return)
		   end)

	val _ = List.foreach
	        (funcs,
		 fn label => enque {label = label, 
				    hints = ([],[])})
	fun loop ()
	  = (case deque ()
	       of NONE => ()
	        | SOME {label, hints}
		=> (doit {label = label, hints = hints};
		    loop ()))
	val _ = loop ()
      in
	T {get = getLiveTransfers,
	   set = setLiveTransfers}
      end

  val computeLiveTransfers 
    = fn {chunk, transferRegs, transferFltRegs, liveInfo, jumpInfo}
       => if !Control.Native.liveTransfer
	    then computeLiveTransfers {chunk = chunk, 
				       transferRegs = transferRegs,
				       transferFltRegs = transferFltRegs,
				       liveInfo = liveInfo, 
				       jumpInfo = jumpInfo}
	    else let
		   val liveTransfers
		    as {get = getLiveTransfers,
			set = setLiveTransfers}
		     = Property.getSetOnce(Label.plist, 
					   Property.initConst (SOME ([], [])))
		 in
		   T {get = getLiveTransfers,
		      set = setLiveTransfers}
		 end

  val (computeLiveTransfers : {chunk : Chunk.t,
			       transferRegs : Register.t list,
			       transferFltRegs : Int.t,
			       liveInfo : LiveInfo.t,
			       jumpInfo : x86JumpInfo.t} -> t,
       computeLiveTransfers_msg)
    = tracerTop
      "computeLiveTransfers"
      computeLiveTransfers

  fun computeLiveTransfers_totals ()
    = (computeLiveTransfers_msg ())
      
  fun getLiveTransfers (T {get, set}, label) = valOf (get label)

  fun setLiveTransfersEmpty (T {get, set}, label) = set(label, SOME ([], []))
end