(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature INFER_TYPE_STRUCTS =
   sig
      include ATOMS
      structure XmlType: XML_TYPE
      sharing Atoms = XmlType.Atoms
   end

signature INFER_TYPE = 
   sig
      include INFER_TYPE_STRUCTS

      structure Frees :
	 sig
	    include SET
	    val fromTyvars: Tyvar.t list -> t
	 end
      
      include TYPE_OPS sharing type tycon = Tycon.t

      (* constructors *)
      val new: {equality: bool, canGeneralize: bool} -> t
      val var: Tyvar.t -> t
      val record: {flexible: bool, record: t SortedRecord.t} -> t
      val ofConst: Ast.Const.t -> t

      (* destructors *)
      val derecord: t -> {flexible: bool, record: t SortedRecord.t}

      (* predicates *)
      val isUnit: t -> bool

      (* the set of free variables of a type *)
      val frees: t -> Frees.t

      (* close(t, f)
       * Make a list of type variables that are free in t, but do not appear in f.
       * Turn any unknown types into new type variables.
       * The second argument expressed as a thunk for performance reasons --
       * it is not necessary to know f if the type is closed.
       *)
      val close: t * (unit -> Frees.t) -> Tyvar.t list

      (* substitute(t, [(a1, t1), ..., (an, tn)]) performs simultaneous
       * substitution of the ti for ai in t.
       *)
      val substitute: t * (Tyvar.t * t) vector -> t

      (* make two types identical (recursively).  side-effecting. *)
      val unify: t * t -> unit
	 
      (* make all types in the list identical.  return new type if empty list. *)
      val unifys: t list -> t 

      (* can two types be unified?  not side-effecting. *)
      val canUnify: t * t -> bool

      (* cached for speed *)
      val toXml: t -> XmlType.t

      val layout: t -> Layout.t
   end
