(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
(* Change any toplevel function that only calls itself in tail position
 * into one with a local loop and no self calls.
 *)
functor IntroduceLoops (S: INTRODUCE_LOOPS_STRUCTS): INTRODUCE_LOOPS = 
struct

open S
open Dec PrimExp Transfer

fun introduceLoops (Program.T {datatypes, globals, functions, main}) =
   let
      val functions =
	 Vector.map
	 (functions, fn Function.T {name, args, body, returns} =>
	  let
	     val callsItself = ref false
	     exception NonTail
	     val noChange = (args, body)
	     val (args, body) =
		(Exp.foreachTransfer
		 (body,
		  fn Call {func, cont, ...} =>
		  if Func.equals (name, func)
		     then (callsItself := true;
			   case cont of
			      NONE => ()
			    | SOME _ => raise NonTail)
		  else ()
			    | _ => ());
		 if !callsItself
		    then
		       let
			  val loopName = Jump.newString "loop"
			  val newArgs =
			     Vector.map (args, fn (x, t) => (Var.new x, t))
			  fun loopTransfer t =
			     case t of
				Call {func, args, cont} =>
				   if Func.equals (name, func)
				      then Jump {dst = loopName,
						 args = args}
				   else t
			      | _ => t
			  fun loopExp e =
			     let val {decs, transfer} = Exp.dest e
			     in Exp.make {decs = List.map (decs, loopDec),
					  transfer = loopTransfer transfer}
			     end
			  and loopDec d =
			     case d of
				Fun {name, args, body} =>
				   Fun {name = name,
					args = args,
					body = loopExp body}
			      | _ => d
		       in (newArgs,
			   Exp.make {decs = [Fun {name = loopName,
						  args = args,
						  body = loopExp body}],
				     transfer =
				     Jump {dst = loopName,
					   args = Vector.map (newArgs, #1)}})
		       end
		 else noChange)
		handle NonTail => noChange
	  in Function.T {name = name,
			 args = args,
			 body = body,
			 returns = returns}
	  end)
   in
      Program.T {datatypes = datatypes,
		 globals = globals,
		 functions = functions,
		 main = main}
   end

end
