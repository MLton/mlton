(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature OBJECT_TYPE =
   sig
      structure PointerTycon: POINTER_TYCON
      structure Runtime: RUNTIME
         
      type ty
      datatype t =
         Array of {elt: ty,
                   hasIdentity: bool}
       | Normal of {hasIdentity: bool,
                    ty: ty}
       | Stack
       | Weak of ty (* in Weak t, must have Type.isPointer t *)
       | WeakGone
         
      val basic: (PointerTycon.t * t) vector
      val isOk: t -> bool
      val layout: t -> Layout.t
      val toRuntime: t -> Runtime.RObjectType.t
   end
