(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
	    val derecord: t * Region.t -> (Record.Field.t * XmlType.t) vector
	    val equals: t * t -> bool
	    val layout: t -> Layout.t
	    val layoutPretty: t -> Layout.t
	    val new: {equality: bool, canGeneralize: bool} -> t
	    val ofConst: Ast.Const.t -> t
	    val record: {flexible: bool,
			 record: t SortedRecord.t,
			 region: Region.t} -> t
	    val toString: t -> string
	    (* cached for speed *)
	    val toXml: t * Region.t -> XmlType.t
	    (* make two types identical (recursively).  side-effecting. *)
	    val unify: t * t * Region.t -> unit
	    (* make all types in the list identical.
	     * return new type if empty list.
	     *)
	    val unifys: t list * Region.t -> t 
	    val var: Tyvar.t -> t
	 end
      sharing type Type.tycon = Tycon.t
	 
      structure InferScheme:
	 sig
	    type t

	    val fromType: Type.t -> t
	    val instantiate: t * Region.t -> {args: unit -> XmlType.t vector,
					      instance: Type.t}
	    val layout: t -> Layout.t
	    val make: {canGeneralize: bool,
		       tyvars: Tyvar.t vector,
		       ty: Type.t} -> t
	    val ty: t -> Type.t
	 end
	 
      structure VarRange:
	 sig
	    datatype kind =
	       Normal
	     | Delayed
	     | Recursive of unit -> XmlType.t vector
	     | Overload of (Var.t * Type.t) vector
	       
	    datatype t = T of {scheme: InferScheme.t,
			       kind: kind}

	    val scheme: t -> InferScheme.t
	    val layout: t -> Layout.t
	 end

      type t

      (* close (e, t, ts, r) = {bound, mayHaveTyvars, scheme}
       * close type t with respect to environment e, including all the tyvars in
       * ts and ensuring than no tyvar in ts occurs free in e.
       * bound returns the vector of type variables in t that do not occur in e,
       * which isn't known until all flexible record fields are determined, after
       * unification is complete.
       *)
      val close:
	 t * Type.t * Tyvar.t vector * Region.t ->
	 {bound: unit -> Tyvar.t vector,
	  mayHaveTyvars: bool,
	  scheme: InferScheme.t}
      val closes:
	 t * Type.t vector * Tyvar.t vector * Region.t
	 -> {bound: unit -> Tyvar.t vector,
	     schemes: InferScheme.t vector}
      val empty: t 
      val extendTyvars: t * Tyvar.t vector -> t
      val extendVar: t * Var.t * InferScheme.t -> t
      val extendVarRange: t * Var.t * VarRange.t -> t
      val layout: t -> Layout.t
      (* Lookup a binding. *)
      val lookupVar: t * Var.t -> InferScheme.t
      val lookupVarRange: t * Var.t -> VarRange.t    
   end

signature INFER_TYPE_ENV = TYPE_ENV
