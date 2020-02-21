(* Copyright (C) 2020 Matthew Fluet
 * Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature OBJECT_TYPE =
   sig
      structure Prod: PROD
      structure ObjptrTycon: OBJPTR_TYCON
      structure Runtime: RUNTIME

      type ty
      datatype t =
         Normal of {components: ty Prod.t,
                    hasIdentity: bool}
       | Sequence of {components: ty Prod.t,
                      hasIdentity: bool}
       | Stack
       | Weak of ty option (* in Weak (SOME t), must have Type.isObjptr t *)

      val basic: unit -> (ObjptrTycon.t * t) vector
      val components: t -> ty Prod.t
      val componentsSize: t -> Bytes.t
      val deNormal: t -> {components: ty Prod.t, hasIdentity: bool}
      val deSequence: t -> {components: ty Prod.t, hasIdentity: bool}
      val isOk: t -> bool
      val layout: t -> Layout.t
      val toRuntime: t -> Runtime.RObjectType.t
   end
