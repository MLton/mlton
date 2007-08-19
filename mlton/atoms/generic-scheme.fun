(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor GenericScheme (S: GENERIC_SCHEME_STRUCTS): GENERIC_SCHEME =
struct

open S

type ty = Type.t
type tyvar = Tyvar.t

datatype t = T of {tyvars: tyvar vector,
                   ty: ty}

local
   fun make f (T r) = f r
in
   val ty = make #ty
end

fun layout (T {tyvars, ty}) =
   let open Layout
      val ty = Type.layout ty
   in
      if 0 = Vector.length tyvars
         then ty
      else
         align [seq [str "Forall ",
                     Vector.layout Tyvar.layout tyvars,
                     str "."],
                ty]
   end

fun apply (T {tyvars, ty}, args) =
   if Vector.isEmpty tyvars andalso Vector.isEmpty args
      then ty (* Must special case this, since don't want to substitute
               * in monotypes.
               *)
   else Type.substitute (ty, Vector.zip (tyvars, args))

end
