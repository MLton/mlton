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
      fun init {array: Var.t,
		numElts: Operand.t,
		profileInfo, statements, transfer}: Transfer.t =
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
			statements = statements,
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
	    Transfer.Goto {args = Vector.new1 (Operand.int 0),
			   dst = loop}
	 end
      val blocks =
	 Vector.map
	 (blocks,
	  fn block as Block.T {args, kind, label, profileInfo, 
			       statements, transfer} =>
	  if 0 = Vector.length statements
	     then block
	  else
	     case Vector.sub (statements, 0) of
		s as Statement.PrimApp {args = arrayArgs, dst, prim, ...} =>
		   let
		      fun doit () =
			 let
			    val transfer =
			       init {array = #1 (valOf dst),
				     numElts = Vector.sub (arrayArgs, 0),
				     profileInfo = profileInfo,
				     statements = (Vector.dropPrefix
						   (statements, 1)),
				     transfer = transfer}
			 in
			    Block.T {args = args,
				     kind = kind,
				     label = label,
				     profileInfo = profileInfo,
				     statements = Vector.new1 s,
				     transfer = transfer}
			 end
		   in
		      case Prim.name prim of
			 Prim.Name.Array_allocate =>
			    (case Vector.sub (arrayArgs, 2) of
				Operand.ArrayHeader {numPointers, ...} =>
				   if numPointers > 0
				      then doit ()
				   else block
			      | _ =>
				   Error.bug "ArrayInit: strange Array_allocate")
		       | _ => block
		   end
	      | _ => block)
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
