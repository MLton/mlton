(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor InferScheme (S: INFER_SCHEME_STRUCTS): INFER_SCHEME = 
struct

open S

local open Type
in
   structure Frees = Frees
   structure Tyvar = Tyvar
end

structure Scheme = GenericScheme (structure Tyvar = Tyvar
				  structure Type = Type)
open Scheme

fun frees (T {tyvars, ty}) =
   Frees.- (Type.frees ty, Frees.fromTyvars (Vector.toList tyvars))

fun instantiate {scheme, canGeneralize} =
   let
      val ts = Vector.map (tyvars scheme, fn v =>
			   Type.new {equality = Tyvar.isEquality v,
				     canGeneralize = canGeneralize})
   in {instance = apply (scheme, ts), args = ts}
   end

end
