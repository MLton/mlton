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

      structure Frees:
	 sig
	    include SET
	    val fromTyvars: Tyvar.t list -> t
	 end
      
      include TYPE_OPS sharing type tycon = Tycon.t

      (* can two types be unified?  not side-effecting. *)
      val canUnify: t * t -> bool
      val derecord: t -> {flexible: bool, record: t SortedRecord.t}
      val frees: t -> Frees.t
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
      (* make all types in the list identical.  return new type if empty list. *)      val unifys: t list -> t 
      val var: Tyvar.t -> t
   end
