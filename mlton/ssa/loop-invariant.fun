(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
(*
 * Remove loop invariant args to local loops.
 * fun loop (x, y) = ... loop (x, z) ...
 *
 * becomes
 *
 * fun loop (x, y') =
 *     let fun loop' (y) = ... loop' (z) ...
 *     in loop' (y')
 *     end
 *)

functor LoopInvariant (S: LOOP_INVARIANT_STRUCTS): LOOP_INVARIANT = 
struct

open S
open Exp Transfer

fun loopInvariant (program as Program.T {globals, datatypes, functions, main}) =
   let
      val shrink = shrinkFunction globals

      fun simplifyFunction f =
	 let
	    val {name, args, start, blocks, returns, mayRaise} = Function.dest f
	    val {get = labelInfo: Label.t -> {callsSelf: bool ref,
					      visited: bool ref,
					      invariant: (Var.t * bool ref) vector,
					      newLabel: Label.t option ref},
		 set = setLabelInfo, ...} =
	       Property.getSetOnce
	       (Label.plist, 
		Property.initRaise ("LoopInvariant.labelInfo", Label.layout))

	    val _ =
	       Vector.foreach
	       (blocks, fn Block.T {label, args, ...} =>
		setLabelInfo (label,
			      {callsSelf = ref false,
			       visited = ref false,
			       invariant = Vector.map (args, fn (x, _) => 
						       (x, ref true)),
			       newLabel = ref NONE}))

	    fun loop (Tree.T (Block.T {label, transfer, ...},
			      children)) =
	       let
		  val {visited, ...} = labelInfo label
	       in
		  visited := true
		  ; case transfer of
		       Goto {dst, args} =>
			  let
			     val {callsSelf, visited, invariant, ...} = labelInfo dst
			  in
			     if !visited
			        then (callsSelf := true
				      ; Vector.foreach2
				        (args, invariant, fn (x, (y, b)) =>
					 if !b andalso not (Var.equals (x, y))
					    then b := false
					 else ()))
			     else ()
			  end
		     | _ => ()
		  ; Vector.foreach (children, loop)
		  ; visited := false
	       end
	    val _ = loop (Function.dfsTree f)

	    fun remove (xs: 'a vector, invariant: ('b * bool ref) vector): 'a vector =
	       Vector.keepAllMap2 (xs, invariant, fn (x, (_, b)) =>
				   if !b then NONE else SOME x)

	    val newBlocks = ref []
	    fun loop (Tree.T (Block.T {label, args, statements, transfer},
			      children)) =
	       let
		  val {callsSelf, invariant, newLabel, ...} = labelInfo label
		  val _ =
		     if !callsSelf
		        andalso Vector.exists (invariant, ! o #2)
		        then newLabel := SOME (Label.new label)
		     else ()
		  val transfer = 
		     case transfer of
		        Goto {dst, args} =>
			   let
			      val {invariant, newLabel, ...} = labelInfo dst
			   in
			      case !newLabel of
				 SOME dst' => Goto {dst = dst',
						    args = remove (args, invariant)}
			       | NONE => transfer
			   end
		      | _ => transfer
		  val (args, statements, transfer) =
		     case !newLabel of
		        NONE => (args, statements, transfer)
		      | SOME label' =>
			   let
			      val label' = valOf (!newLabel)
			      val _ =
				 Control.diagnostic
				 (fn () =>
				  let open Layout
				  in seq [Label.layout label,
					  str " -> ",
					  Label.layout label']
				  end)
			      val (outerFormals,
				   innerFormals,
				   innerActuals) =
				 Vector.foldr2
				 (args, invariant, ([], [], []),
				  fn ((x, t), (_, b), (ofs, ifs, ias)) =>
				  if !b
				     then ((x, t) :: ofs, ifs, ias)
				  else let val x' = Var.new x
				       in ((x', t) :: ofs,
					   (x, t) :: ifs,
					   x' :: ias)
				       end)
			   in
			      List.push
			      (newBlocks, 
			       Block.T {label = label',
					args = Vector.fromList innerFormals,
					statements = statements,
					transfer = transfer})
			      ; (Vector.fromList outerFormals, 
				 Vector.new0 (),
				 Goto {dst = label',
				       args = Vector.fromList innerActuals})
			   end
		  val _ = List.push
		          (newBlocks,
			   Block.T {label = label,
				    args = args,
				    statements = statements,
				    transfer = transfer})
		  val _ = Vector.foreach (children, loop)
		  val _ = newLabel := NONE
	       in
		  ()
	       end
	    val _ = loop (Function.dfsTree f)
	    val blocks = Vector.fromList (!newBlocks)
	    val f = Function.new {name = name,
				  args = args,
				  start = start,
				  blocks = blocks,
				  returns = returns,
				  mayRaise = mayRaise}
	    val _ = Function.clear f
	 in
	    f
	 end
      val functions = List.revMap (functions, shrink o simplifyFunction)
      val program = 
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = functions,
		    main = main}
      val _ = Program.clearTop program
   in
      program
   end
end
