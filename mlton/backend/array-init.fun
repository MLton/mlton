(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
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
	    Statement.PrimApp {prim, args, ...} =>
	       (case Prim.name prim of
		   Prim.Name.Array_allocate =>
		      let
			 fun error () = Error.bug "Array_allocate without header"
			 val header = case (Vector.sub (args, 2)) of
			                 Operand.Const c =>
					    (case Const.node c of
					        Const.Node.Word w => w
					      | _ => error ())
				       | _ => error ()
			 val {numPointers, ...} = Runtime.splitArrayHeader header
		      in
			 numPointers > 0
		      end
		 | _ => false)
	  | _ => false
      fun needsSplit s =
	 case s of
	    Statement.PrimApp {prim, ...} =>
	       isSome (Prim.bytesNeeded prim) andalso (Prim.impCall prim)
	  | _ => false
      fun needsRewrite s =
	 (needsInit s,  needsSplit s)
      fun needsRewrite' s = let val (b1, b2) = needsRewrite s in b1 orelse b2 end

      fun insertSplit (s,
		       profileInfo,
		       statements, transfer) =
	 let
	    fun error () = Error.bug "non PrimApp to insertSplit"
	    val (prim, dst, args) = 
	       case s of
		  Statement.PrimApp {prim, dst, args} => (prim, dst, args)
		| _ => error ()
	    val continue = Label.newNoname ()
	    val _ = 
	       extra :=
	       Block.T {args = case dst
				 of SOME dst => Vector.new1 dst
				  | NONE => Vector.new0 (),
			kind = Kind.CReturn {prim = prim},
			label = continue,
			profileInfo = profileInfo,
			statements = Vector.fromList statements,
			transfer = transfer}
	       :: !extra
				  
	 in
	    ([],
	     Transfer.CCall {args = args,
			     prim = prim,
			     return = continue,
			     returnTy = Option.map (dst, #2)})
	 end
      fun insertInit (s, 
		      profileInfo,
		      statements, transfer) =
	 let
	    fun error () = Error.bug "non Array_allocate to insertInit"
	    val (array, numElts) =
	       case s of
		  Statement.PrimApp {prim, dst, args, ...} =>
		     let
		        val _ = case Prim.name prim of
			           Prim.Name.Array_allocate => ()
				 | _ => error ()
			val array = case dst of
			               SOME (array, _) => array
				     | _ => error ()
			val numElts = Vector.sub(args, 0)
		     in
		        (array, numElts)
		     end
		| _ => error ()
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
	    ([s],
	     Transfer.Goto {args = Vector.new1 (Operand.int 0),
			    dst = loop})
	 end
      val blocks =
	 Vector.map
	 (blocks,
	  fn block as Block.T {args, kind, label, profileInfo, 
			       statements, transfer} =>
	  if not (Vector.exists (statements, needsRewrite'))
	     then block
	  else
	     let
		val (statements, transfer) =
		   Vector.foldr
		   (statements, ([], transfer), fn (s, (statements, transfer)) =>
		    case needsRewrite s of
		       (true, false) => insertInit (s, profileInfo, statements, transfer)
		     | (false, true) => insertSplit (s, profileInfo, statements, transfer)
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
