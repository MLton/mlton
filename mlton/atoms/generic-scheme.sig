(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature GENERIC_SCHEME_STRUCTS = 
   sig
      structure Tyvar: TYVAR
      structure Type: sig
                          type t
                          val var: Tyvar.t -> t
                          val substitute: t * (Tyvar.t * t) vector -> t
                          val layout: t -> Layout.t
                       end
   end

signature GENERIC_SCHEME = 
   sig
      type tyvar
      type ty

      datatype t = T of {tyvars: tyvar vector,
                         ty: ty}

      val apply: t * ty vector -> ty
      val layout: t -> Layout.t
      val ty: t -> ty
   end
