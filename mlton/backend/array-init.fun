functor ArrayInit (S: ARRAY_INIT_STRUCTS): ARRAY_INIT = 
struct

open S
open Rssa

fun insertFunction (f: Function.t) =
   let
      val {args, blocks, name, start} = Function.dest f
      val extra = ref []
      fun needsInit s =
	 case s of
	    Statement.Array {numPointers, ...} => numPointers > 0
	  | _ => false
      fun insertInit (z as {dst = array, numElts, ...}, 
		      profileInfo,
		      statements, transfer) =
	 let
	    val continue = Label.newNoname ()
	    val loop = Label.newString "initLoop"
	    val loopi' = Label.newNoname ()
	    val i = Var.newNoname ()
	    val i' = Var.newNoname ()
	    val isNumElts = Var.newNoname ()
	    val loopStatements =
	       Vector.new3
	       (Statement.Move
		{dst = Operand.ArrayOffset {base = array,
					    index = i,
					    ty = Type.pointer},
		 src = Operand.Pointer 1},
		Statement.PrimApp
		{args = Vector.new2 (Operand.Var {var = i, ty = Type.int},
				     Operand.int 1),
		 dst = SOME (i', Type.int),
		 prim = Prim.intAdd},
		Statement.PrimApp
		{args = Vector.new2 (numElts,
				     Operand.Var {var = i', ty = Type.int}),
		 dst = SOME (isNumElts, Type.bool),
		 prim = Prim.eq})
	    val loopTransfer =
	       Transfer.iff (Operand.Var {var = isNumElts, ty = Type.bool},
			     {falsee = loopi',
			      truee = continue})
	    val _ =
	       extra :=
	       Block.T {args = Vector.new0 (),
			kind = Kind.Jump,
			label = continue,
			profileInfo = profileInfo,
			statements = Vector.fromList statements,
			transfer = transfer}
	       :: Block.T {args = Vector.new1 (i, Type.int),
			   kind = Kind.Jump,
			   label = loop,	
			   profileInfo = profileInfo,
			   statements = loopStatements,
			   transfer = loopTransfer}
	       :: Block.T {args = Vector.new0 (),
			   kind = Kind.Jump,
			   label = loopi',
			   profileInfo = profileInfo,
			   statements = Vector.new0 (),
			   transfer =
			   Transfer.Goto
			   {args = Vector.new1 (Operand.Var {var = i',
							     ty = Type.int}),
			    dst = loop}}
	       :: !extra
	 in
	    ([Statement.Array z],
	     Transfer.Goto {args = Vector.new1 (Operand.int 0),
			    dst = loop})
	 end
      val blocks =
	 Vector.map
	 (blocks,
	  fn block as Block.T {args, kind, label, profileInfo, 
			       statements, transfer} =>
	  if not (Vector.exists (statements, needsInit))
	     then block
	  else
	     let
		val (statements, transfer) =
		   Vector.foldr
		   (statements, ([], transfer), fn (s, (statements, transfer)) =>
		    case s of
		       Statement.Array (z as {numPointers, ...}) =>
		         if numPointers > 0
			    then
			       insertInit (z, profileInfo, statements, transfer)
			 else (s :: statements, transfer)
		     | _ => (s :: statements, transfer))
	     in
		Block.T {args = args,
			 kind = kind,
			 label = label,
			 profileInfo = profileInfo,
			 statements = Vector.fromList statements,
			 transfer = transfer}
	     end)
      val blocks = Vector.concat [blocks, Vector.fromList (!extra)]
   in
      Function.new {args = args,
		    blocks = blocks,
		    name = name,
		    start = start}
   end

fun insert (Program.T {functions, main}) =
   Program.T {functions = List.revMap (functions, insertFunction),
	      main = insertFunction main}


end
