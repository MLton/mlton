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
	    val isZero = Var.newNoname ()
	    val compare =
	       Statement.PrimApp
	       {args = Vector.new2 (Operand.Var {var = numElts, ty = Type.int},
				    Operand.int 0),
		dst = SOME (isZero, Type.bool),
		prim = Prim.eq}
	    val continue = Label.newNoname ()
	    val init = Label.newNoname ()
	    val loop = Label.newNoname ()
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
		{args = Vector.new2 (Operand.Var {var = numElts, ty = Type.int},
				     Operand.Var {var = i', ty = Type.int}),
		 dst = SOME (isNumElts, Type.bool),
		 prim = Prim.eq})
	    val loopTransfer =
	       Transfer.Switch
	       {cases = Cases.Int [(0, loopi'), (1, continue)],
		default = NONE,
		test = Operand.Var {var = isNumElts, ty = Type.bool}}
	    val maybeInit =
	       Transfer.Switch
	       {cases = Cases.Int [(0, init), (1, continue)],
		default = NONE,
		test = Operand.Var {var = isZero, ty = Type.bool}}
	    val _ =
	       extra :=
	       Block.T {args = Vector.new0 (),
			kind = Kind.Jump,
			label = init,
			statements = Vector.new0 (),
			profileInfo = profileInfo,
			transfer = (Transfer.Goto
				    {args = Vector.new1 (Operand.int 0),
				     dst = loop})}
	       :: Block.T {args = Vector.new0 (),
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
	    ([Statement.Array z, compare], maybeInit)
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
		       Statement.Array (z as {numBytesNonPointers, ...}) =>
		         if numBytesNonPointers > 0
			    then (s :: statements, transfer)
			 else insertInit (z, profileInfo, statements, transfer)
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
