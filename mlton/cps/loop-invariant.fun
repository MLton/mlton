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
open Dec PrimExp Transfer

fun introExp (e: Exp.t): Exp.t =
   let
      val {get = jumpInfo: Jump.t -> {
				      callsSelf: bool ref,
				      inScope: bool ref,
				      invariant: (Var.t * bool ref) vector,
				      newName: Jump.t option ref
				      },
	   set = setJumpInfo} =
	 Property.getSetOnce
	 (Jump.plist, Property.initRaise ("LoopInvariant.info", Jump.layout))
      fun loopTransfer t =
	 case t of
	    Jump {dst, args} =>
	       let val {callsSelf, inScope, invariant, ...} = jumpInfo dst
	       in if !inScope
		     then (callsSelf := true
			   ; Vector.foreach2 (args, invariant, fn (x, (y, b)) =>
					     if !b
						andalso not (Var.equals (x, y))
						then b := false
					     else ()))
		  else ()
	       end
	  | _ => ()

      fun loopExp e =
	 let val {decs, transfer} = Exp.dest e
	 in List.foreach (Exp.decs e, loopDec)
	    ; loopTransfer transfer
	 end
      and loopDec d =
	 case d of
	    Fun {name, args, body} =>
	       let val inScope = ref false
	       in setJumpInfo (name,
			       {callsSelf = ref false,
				invariant = Vector.map (args, fn (x, _) =>
						       (x, ref true)),
				inScope = inScope,
				newName = ref NONE})
		  ; inScope := true
		  ; loopExp body
		  ; inScope := false
	       end
	  | _ => ()
      val _ = loopExp e
      fun remove (xs: 'a vector, inv: ('b * bool ref) vector): 'a vector =
	 Vector.keepAllMap2 (xs, inv, fn (x, (_, b)) =>
			     if !b then NONE else SOME x)
      fun loopTransfer t =
	 case t of
	    Jump {dst, args} =>
	       let val {newName, invariant, ...} = jumpInfo dst
	       in case !newName of
		  NONE => t
		| SOME dst =>
		     Jump {dst = dst,
			   args = remove (args, invariant)}
	       end
	  | _ => t
	       
      fun loopExp e =
	 let val {decs, transfer} = Exp.dest e
	 in Exp.make {decs = List.map (decs, loopDec),
		      transfer = loopTransfer transfer}
	 end
      and loopDec d =
	 case d of
	    Fun {name, args, body} =>
	       let
		  val {newName, callsSelf, invariant, ...} =
		     jumpInfo name
		  val (args, body) =
		     if !callsSelf
			andalso Vector.exists (invariant, ! o #2)
			then let val name' = Jump.new name
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
				 val _ = newName := SOME name'
				 val body =
				    Exp.make
				    {decs = [Fun {name = name',
						  args = Vector.fromList innerFormals,
						  body = loopExp body}],
				     transfer = Jump {dst = name',
						      args = Vector.fromList innerActuals}}
			     in newName := NONE;
				(Vector.fromList outerFormals, body)
			     end
		     else (args, loopExp body)
	       in Fun {name = name, args = args, body = body}
	       end
	  | _ => d

      val e = loopExp e
	 
   in e
   end

val loopInvariant = simplifyProgram introExp

end
