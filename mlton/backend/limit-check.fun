functor LimitCheck (S: LIMIT_CHECK_STRUCTS): LIMIT_CHECK =
struct

open S
open Rssa

fun insertFunction (f: Function.t) =
   let
      val {args, blocks, name, start} = Function.dest f
      val extra = ref []
      fun add {args, blockKind, lcKind, start, success} =
	 let
	    val failure = Label.newNoname ()
	 in
	    extra :=
	    Block.T {args = args,
		     kind = blockKind,
		     label = start,
		     statements = Vector.new0 (),
		     transfer = Transfer.LimitCheck {failure = failure,
						     kind = lcKind,
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
				Statement.Object
				{numPointers = p,
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
				blockKind = kind,
				lcKind = lcKind,
				start = label,
				success = success}
		in
		   Block.T {args = Vector.new0 (),
			    kind = Kind.Jump,
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
			    (if bytesPerElt = 0
				then LimitCheck.Heap {bytes = extraBytes,
						      stackToo = false}
			     else LimitCheck.Array {bytesPerElt = bytesPerElt,
						    extraBytes = extraBytes,
						    numElts = numElts,
						    stackToo = false})
			 end
                    | _ => normal ()
	     else normal ()
	  end)
      val newStart = Label.newNoname ()
      val _ = add {args = Vector.new0 (),
		   blockKind = Kind.Jump,
		   lcKind = LimitCheck.Stack,
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
	      main = insertFunction main}

end
