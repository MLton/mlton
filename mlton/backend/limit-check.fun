functor LimitCheck (S: LIMIT_CHECK_STRUCTS): LIMIT_CHECK =
struct

open S
open Rssa

fun insertFunction (f: Function.t) =
   let
      val {args, blocks, name, start} = Function.dest f
      val extra = ref []
      fun add {args, kind, start, success} =
	 let
	    val failure = Label.newNoname ()
	 in
	    extra :=
	    Block.T {args = args,
		     kind = Kind.Jump,
		     label = start,
		     statements = Vector.new0 (),
		     transfer = Transfer.LimitCheck {failure = failure,
						     kind = kind,
						     success = success}}
	    :: Block.T {args = Vector.new0 (),
			kind = Kind.Runtime {prim = Prim.gcCollect},
			label = failure,
			statements = Vector.new0 (),
			transfer = Transfer.Goto {dst = success,
						  args = Vector.new0 ()}}
	    :: !extra
	 end
      val blocks =
	 Vector.map
	 (blocks,
	  fn block as Block.T {args, kind, label, statements, transfer} =>
	  let
	     val bytes = 
		Vector.fold (statements, 0, fn (s, ac) =>
			     case s of
				Statement.Object {numPointers = p,
						  numWordsNonPointers = np, ...} =>
				   ac + Runtime.objectHeaderSize
				   + (Runtime.objectSize
				      {numPointers = p,
				       numWordsNonPointers = np})
			      | _ => ac)
	     fun insert (lcKind: LimitCheck.t) =
		let
		   val success = Label.newNoname ()
		   val _ = add {args = args,
				kind = lcKind,
				start = label,
				success = success}
		in
		   Block.T {args = Vector.new0 (),
			    kind = kind,
			    label = success,
			    statements = statements,
			    transfer = transfer}
		end
	     fun normal () =
		if bytes > 0
		   then insert (LimitCheck.Heap {bytes = bytes,
						 stackToo = false})
		else block
	  in
	     if 0 < Vector.length statements
		then
		   case Vector.sub (statements, 0) of
		      Statement.Array {dst, numBytesNonPointers, numElts,
				       numPointers} =>
			 let
			    val bytesPerElt =
			       if numPointers = 0
				  then numBytesNonPointers
			       else if numBytesNonPointers = 0
				       then numPointers * Runtime.pointerSize
				    else Error.unimplemented "tricky arrays"
			    val extraBytes =
			       bytes
			       + Runtime.arrayHeaderSize
			       (* Spare for forwarding pointer for zero length
				* arrays.
				*)
			       + Runtime.pointerSize
			 in
			    insert
			    (LimitCheck.Array {bytesPerElt = bytesPerElt,
					       extraBytes = bytes,
					       numElts = numElts,
					       stackToo = false})
			 end
                    | _ => normal ()
	     else normal ()
	  end)
      val newStart = Label.newNoname ()
      val _ = add {args = Vector.new0 (),
		   kind = LimitCheck.Stack,
		   start = newStart,
		   success = start}
      val blocks = Vector.concat [blocks, Vector.fromList (!extra)]
   in
      Function.new {args = args,
		    blocks = blocks,
		    name = name,
		    start = newStart}
   end
	 
fun insert (Program.T {functions, main}) =
   Program.T {functions = List.revMap (functions, insertFunction),
	      main = main}

(* fun allocateArray
 * 			      ({user = {gcInfo, numElts, numPointers,
 * 					numBytesNonPointers, ...},
 * 				limitCheck}: Statement.allocateArray,
 * 			       bytesAllocated: int): int =
 * 			      let
 * 				 val bytesAllocated =
 * 				    bytesAllocated
 * 				    (* space for array header *)
 * 				    + arrayHeaderSize
 * 				 (* maxArrayLimitCheck is arbitrary -- it's just
 * 				  * there to ensure that really huge array
 * 				  * allocations don't get moved too early.
 * 				  *)
 * 				 val maxArrayLimitCheck = 10000
 * 			      in
 * 				 case numElts of
 * 				    Operand.Int numElts =>
 * 				       if numElts <= maxArrayLimitCheck
 * 					  then
 * 					     (bytesAllocated +
 * 					      Type.align (Type.pointer,
 * 							  numElts * bytesPerElt))
 * 					     handle Exn.Overflow => here ()
 * 				       else here ()
 * 				  | _ => here ()
 * 			      end
 *)

end
