(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor DeadCode (S: DEAD_CODE_STRUCTS): DEAD_CODE = 
struct

open S
open CoreML
open Pat Dec Exp

fun deadCode {basis, user} =
   let
      val {get = varIsUsed, set = setVarIsUsed, destroy, ...} =
	 Property.destGetSet (Var.plist, Property.initConst false)
      fun foreachDefinedVar (d, f) =
	 case d of
	    Val {pat, ...} => Pat.foreachVar (pat, f)
	  | Fun {decs, ...} => Vector.foreach (decs, f o #var)
	  | Overload {var, ...} => f var
	  | _ => ()

      fun patVarIsUsed p =
	 DynamicWind.withEscape
	 (fn escape =>
	  (Pat.foreachVar (p, fn x => if varIsUsed x
					 then escape true
				      else ())
	   ; false))
	 
      fun decIsNeeded d =
	 case d of
	    Val {pat, ...} =>
	       (case pat of
		   Wild => true
		 | _ => patVarIsUsed pat)
	  | Fun {decs, ...} => Vector.exists (decs, varIsUsed o #var)
	  | Datatype _ => true
	  | Exception _ => true
	  | Overload {var, ...} => varIsUsed var

      fun useVar x = setVarIsUsed (x, true)
      fun useExp e = Exp.foreachVar (e, useVar)
      fun useDec d = 
	 case d of
	    Val {exp, ...} => useExp exp
	  | Fun {decs, ...} =>
	       Vector.foreach (decs, fn {rules, ...} =>
			       Vector.foreach (rules, useExp o #2))
	  | Datatype _ => ()
	  | Exception _ => ()
	  | Overload {ovlds, ...} => Vector.foreach (ovlds, useVar)

      fun decIsWild d =
	 case d of
	    Val {pat = Wild, ...} => true
	  | _ => false
      val _ = List.foreach (user, useDec)
      val _ = List.foreach (basis, fn d => if decIsWild d then useDec d else ())
      val res =
	 List.fold (rev basis, [], fn (d, b) =>
		    if decIsNeeded d
		       then (useDec d; d :: b)
		    else b)
      val _ = destroy ()
   in res
   end

end
