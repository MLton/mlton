(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor DeadCode (S: DEAD_CODE_STRUCTS): DEAD_CODE = 
struct

open S
open CoreML
open Dec

fun deadCode {basis, user} =
   let
      val {get = varIsUsed, set = setVarIsUsed, destroy, ...} =
	 Property.destGetSet (Var.plist, Property.initConst false)
      fun foreachDefinedVar (d: Dec.t, f) =
	 let

	 in
	    case d of
	       Fun {decs, ...} => Vector.foreach (decs, f o #var)
	     | Val {rvbs, vbs, ...} =>
		  (Vector.foreach (rvbs, f o #var)
		   ; Vector.foreach (vbs, fn {pat, ...} =>
				     Pat.foreachVar (pat, f)))
	     | _ => ()
	 end
      fun patVarIsUsed (p: Pat.t): bool =
	 DynamicWind.withEscape
	 (fn escape =>
	  (Pat.foreachVar (p, fn x => if varIsUsed x
					 then escape true
				      else ())
	   ; false))
      fun decIsNeeded (d: Dec.t): bool =
	 case d of
	    Datatype _ => true
	  | Exception _ => true
	  | Fun {decs, ...} => Vector.exists (decs, varIsUsed o #var)
	  | Val {rvbs, vbs, ...} =>
	       Vector.exists (rvbs, varIsUsed o #var)
	       orelse Vector.exists (vbs, fn {pat, ...} =>
				     Pat.isWild pat orelse patVarIsUsed pat)
      fun useVar x = setVarIsUsed (x, true)
      fun useExp (e: Exp.t): unit = Exp.foreachVar (e, useVar)
      fun useLambda (l: Lambda.t): unit =
	 useExp (#body (Lambda.dest l))
      fun useDec (d: Dec.t): unit = 
	 case d of
	    Datatype _ => ()
	  | Exception _ => ()
	  | Fun {decs, ...} => Vector.foreach (decs, useLambda o #lambda)
	  | Val {rvbs, vbs, ...} =>
	       (Vector.foreach (rvbs, useLambda o #lambda)
		; Vector.foreach (vbs, useExp o #exp))
      fun decIsWild (d: Dec.t): bool =
	 case d of
	    Val {rvbs, vbs, ...} =>
	       0 = Vector.length rvbs
	       andalso 1 = Vector.length vbs
	       andalso Pat.isWild (#pat (Vector.sub (vbs, 0)))
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
