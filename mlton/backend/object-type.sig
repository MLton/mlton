(* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature OBJECT_TYPE =
   sig
      structure ObjptrTycon: OBJPTR_TYCON
      structure Runtime: RUNTIME

      type ty
      datatype t =
         Array of {elt: ty,
                   hasIdentity: bool}
       | Normal of {hasIdentity: bool,
                    ty: ty}
       | Stack
       | Weak of ty option (* in Weak (SOME t), must have Type.isPointer t *)

      val basic: unit -> (ObjptrTycon.t * t) vector
      val isOk: t -> bool
      val layout: t -> Layout.t
      val toRuntime: t -> Runtime.RObjectType.t
   end
