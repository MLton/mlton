(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor LoopCount(S: LOOP_COUNT_STRUCTS): LOOP_COUNT = 
struct

open S
open Dec PrimExp Transfer

local
   fun make(name: string): unit -> Dec.t =
      let val prim = Prim.newNullary name
      in fn () => Bind{var = Var.newNoname(),
		       ty = Type.unit,
		       exp = PrimApp{prim = prim, targs = [], args = []}}
      end
in
  val loopCount = make "MLTON_incCpsLoop"
  val callCount = make "MLTON_incCpsCall"
  val dispatchCount = make "MLTON_incCpsDispatch"
  val coerceCount = make "MLTON_incCpsCoerce"
end

fun instrument(program as Program.T{datatypes, globals, functions, main}) =
   if !Control.instrument
      then
	 let
	    val {get: Jump.t -> {inScope: bool ref,
				 isLoop: bool ref},
		 set} =
	       Property.new(Jump.plist, Property.initRaise("info", Jump.layout))
		   
	    fun loopExp(e: Exp.t): Exp.t =
	       let val {decs, transfer} = Exp.dest e
		  fun loopDecs(ds: Dec.t list): Dec.t list =
		     case ds of
			[] => (case transfer of
				  Jump{dst, ...} =>
				     let val {inScope, isLoop} = get dst
				     in if !inScope then isLoop := true else ()
				        ; []
				     end
				| Call _ => [callCount()]
				| Case{cause, ...} =>
				     (case cause of
					 Cause.Dispatch => [dispatchCount()]
				       | Cause.Coerce => [coerceCount()]
				       | _ => [])
				| _ => [])
		      | d :: ds =>
			   case d of
			      Fun{name, args, body} =>
				 let val isLoop = ref false
				    val inScope = ref true
				    val _ = set(name, {isLoop = isLoop,
						       inScope = inScope})
				    val body = loopExp body
				    val _ = inScope := false
				 in Fun{name = name, args = args,
					body =
					if !isLoop
					   then Exp.prefix(body, loopCount())
					else body}
				    :: loopDecs ds
				 end
			    | _ => d :: loopDecs ds
	       in Exp.make{decs = loopDecs decs,
			   transfer = transfer}
	       end
	    val program =
	       Program.T{datatypes = datatypes,
			 globals = globals,
			 functions = List.map(functions,
					      fn {name, args, body, returns} =>
					      {name = name,
					       args = args,
					       body = loopExp body,
					       returns = returns}),
			 main = main}
	 in Program.clear program
	    ; program
	 end
   else program

end
