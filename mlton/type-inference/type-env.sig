(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature TYPE_ENV_STRUCTS = 
   sig
      include ATOMS
      structure XmlType: XML_TYPE
      sharing Atoms = XmlType.Atoms
   end

signature TYPE_ENV = 
   sig
      include TYPE_ENV_STRUCTS
      
      structure InferScheme: INFER_SCHEME
      structure InferType: INFER_TYPE
      sharing Atoms = InferType.Atoms
      sharing InferType = InferScheme.Type
	 
      structure VarRange:
	 sig
	    datatype kind =
	       Normal
	     | Delayed
	     | Recursive of Tyvar.t vector ref
	     | Overload of (Var.t * InferType.t) vector
	       
	    datatype t = T of {scheme: InferScheme.t,
			       kind: kind}

	    val scheme: t -> InferScheme.t
	    val layout: t -> Layout.t
	 end

      type t

      val empty: t 
      val extendVar: t * Var.t * InferScheme.t -> t
      val extendVarRange: t * Var.t * VarRange.t -> t
      (* Frees returns the union of the frees of all of the range elements. *)
      val frees: t -> InferType.Frees.t 
      val layout: t -> Layout.t
      (* Lookup a binding. *)
      val lookupVar: t * Var.t -> InferScheme.t
      val lookupVarRange: t * Var.t -> VarRange.t    
   end

signature INFER_TYPE_ENV = TYPE_ENV
