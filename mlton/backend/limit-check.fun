(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
(* Insert limit checks at
 * 1. Loop headers
 * 2. Continuations
 * 3. Handlers
 *)
functor LimitCheck (S: LIMIT_CHECK_STRUCTS): LIMIT_CHECK = 
struct

open S
open Dec Transfer

datatype t = No | Maybe | Yes

val op < =
   fn (Yes, _) => false
    | (Maybe, Yes) => true
    | (No, No) => false
    | (No, _) => true
    | _ => false

fun limitCheck (program as Program.T {functions, ...}) =
   let
      val usesSignals =
	 Program.hasPrim (program, fn p =>
			  Prim.name p = Prim.Name.Thread_finishHandler)
      val {get = jumpInfo: Jump.t -> {inBody: bool ref,
				      limitCheck: t ref},
	   destroy} =
	 Property.destGet (Jump.plist,
			   Property.initFun (fn _ => {inBody = ref false,
						      limitCheck = ref No}))
      fun up ({inBody, limitCheck = r}, v: t) =
	 if !r < v
	    then r := v
	 else ()
      fun jump j =
	 let val info as {inBody, ...} = jumpInfo j
	 in if !inBody
	       then if usesSignals
		       then up (info, Yes)
		    else up (info, Maybe)
	    else ()
	 end	 
      fun loopExp e =
	 let val {decs, transfer} = Exp.dest e
	 in List.foreach (Exp.decs e,
			  fn Fun {name, body, ...} =>
			  let val {inBody = r, ...} = jumpInfo name
			  in r := true
			     ; loopExp body
			     ; r := false
			  end
			   | HandlerPush h => up (jumpInfo h, Maybe)
			   | _ => ())
	    ; (case transfer of
		  Jump {dst, ...} => jump dst
		| Case {cases, default, ...} =>
		     (Cases.foreach (cases, jump)
		      ; Option.app (default, jump))
		| _ => ())
	 end
      val _ = Vector.foreach (functions, loopExp o #body)
   in
      {destroy = destroy,
       limitCheck = ! o #limitCheck o jumpInfo}
   end
   
end


