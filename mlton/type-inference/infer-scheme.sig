(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature INFER_SCHEME_STRUCTS = 
   sig
      structure Type: INFER_TYPE
   end

signature INFER_SCHEME = 
   sig
      include INFER_SCHEME_STRUCTS
      include GENERIC_SCHEME
      sharing type tyvar = Type.Tyvar.t
      sharing type ty = Type.t

      structure Frees: SET sharing Frees = Type.Frees
      
      (* frees returns the set of free variables of a scheme, i.e.
       * the frees of the body minus the bound tyvars.
       *)
(*      val frees: t -> Frees.t *)
      (* Create a new copy of of the body of the scheme, replacing
       * the bound tyvars with new unknown types of the same equality
       * as the tyvar and the given canGeneralize.
       *)
      val instantiate:
	 {scheme: t,
	  canGeneralize: bool} -> {args: Type.t vector,
				   instance: Type.t}
      (* true if the scheme may contain an unknown *)
      val mayContainUnknown: t -> bool
   end
