(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
      val equals: t * t -> bool
      val fromType: ty -> t
      val layout: t -> Layout.t
      val make0: ty -> t
      val make1: (ty -> ty) -> t
      val make2: (ty * ty -> ty) -> t
      val makeEqual1: (ty -> ty) -> t
      val ty: t -> ty
      val tyvars: t -> tyvar vector
   end
