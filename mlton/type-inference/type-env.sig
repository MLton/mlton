(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
   
signature TYPE_ENV_STRUCTS = 
   sig
      include ATOMS
      structure XmlType: XML_TYPE
      sharing Atoms = XmlType.Atoms
   end

signature TYPE_ENV = 
   sig
      include TYPE_ENV_STRUCTS
      
      structure Type:
	 sig
	    include TYPE_OPS

            (* can two types be unified?  not side-effecting. *)
            val canUnify: t * t -> bool
	    val derecord: t -> {flexible: bool,
				record: t SortedRecord.t}
	    val isUnit: t -> bool
	    val layout: t -> Layout.t
	    val new: {equality: bool, canGeneralize: bool} -> t
	    val ofConst: Ast.Const.t -> t
	    val record: {flexible: bool, record: t SortedRecord.t} -> t
	    (* substitute(t, [(a1, t1), ..., (an, tn)]) performs simultaneous
	     * substitution of the ti for ai in t.
	     *)
	    val substitute: t * (Tyvar.t * t) vector -> t
	    (* cached for speed *)
	    val toXml: t -> XmlType.t
	    (* make two types identical (recursively).  side-effecting. *)
	    val unify: t * t -> unit
	    (* make all types in the list identical.
	     * return new type if empty list.
	     *)
	    val unifys: t list -> t 
	    val var: Tyvar.t -> t
	 end
      sharing type Type.tycon = Tycon.t
	 
      structure InferScheme:
	 sig
	    include GENERIC_SCHEME
	       
	    val instantiate:
	       {scheme: t,
		canGeneralize: bool} -> {args: Type.t vector,
					 instance: Type.t}
	 end
      sharing type InferScheme.tyvar = Tyvar.t
      sharing type InferScheme.ty = Type.t
	 
      structure VarRange:
	 sig
	    datatype kind =
	       Normal
	     | Delayed
	     | Recursive of Tyvar.t vector ref
	     | Overload of (Var.t * Type.t) vector
	       
	    datatype t = T of {scheme: InferScheme.t,
			       kind: kind}

	    val scheme: t -> InferScheme.t
	    val layout: t -> Layout.t
	 end

      type t

      (* close (e, t, ts) close type t with respect to environment e, and ensure
       * that no variable in ts occurs free in e.
       *)
      val close: t * Type.t * Tyvar.t vector -> Tyvar.t list
      val empty: t 
      val extendVar: t * Var.t * InferScheme.t -> t
      val extendVarRange: t * Var.t * VarRange.t -> t
      val layout: t -> Layout.t
      (* Lookup a binding. *)
      val lookupVar: t * Var.t -> InferScheme.t
      val lookupVarRange: t * Var.t -> VarRange.t    
   end

signature INFER_TYPE_ENV = TYPE_ENV
