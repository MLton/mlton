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
   end

signature TYPE_ENV = 
   sig
      include TYPE_ENV_STRUCTS
      structure Type:
	 sig
	    include TYPE_OPS

	    val admitsEquality: t -> bool
            (* can two types be unified?  not side-effecting. *)
            val canUnify: t * t -> bool
	    val char: t
	    val deEta: t * Tyvar.t vector -> Tycon.t option
	    val deRecord: t -> (Record.Field.t * t) vector
	    val flexRecord: t SortedRecord.t -> t * (unit -> bool)
	    datatype expandOpaque =
	       Always
	     | Never
	     | Sometimes of Tycon.t -> bool
	    val expandOpaque: t * expandOpaque -> t
	    val hom: t * {con: Tycon.t * 'a vector -> 'a,
			  expandOpaque: expandOpaque,
			  record: 'a SortedRecord.t -> 'a,
			  var: Tyvar.t -> 'a} -> 'a
	    val isChar: t -> bool
	    val isUnit: t -> bool
	    val layout: t -> Layout.t
	    val layoutPretty: t -> Layout.t
	    val makeHom: {con: Tycon.t * 'a vector -> 'a,
			  expandOpaque: expandOpaque,
			  var: Tyvar.t -> 'a} -> {destroy: unit -> unit,
						  hom: t -> 'a}
	    val makeLayoutPretty:
	       unit -> {destroy: unit -> unit,
			lay: t -> Layout.t * {isChar: bool,
					      needsParen: bool}}
	    val new: unit -> t
	    val record: t SortedRecord.t -> t
	    val string: t
	    val toString: t -> string
	    (* make two types identical (recursively).  side-effecting. *)
	    val unify:
	       t * t * (unit -> unit)
	       * (Layout.t * Layout.t -> Region.t * Layout.t * Layout.t)
	       -> unit 
	    val unresolvedInt: unit -> t
	    val unresolvedReal: unit -> t
	    val unresolvedWord: unit -> t
	    val var: Tyvar.t -> t
	 end
      sharing type Type.intSize = IntSize.t
      sharing type Type.realSize = RealSize.t
      sharing type Type.wordSize = WordSize.t
      sharing type Type.tycon = Tycon.t
      structure Scheme:
	 sig
	    type t

	    val admitsEquality: t -> bool
	    val apply: t * Type.t vector -> Type.t
	    val bound: t -> Tyvar.t vector
	    val dest: t -> Tyvar.t vector * Type.t
	    val fromType: Type.t -> t
	    val haveFrees: t vector -> bool vector
	    val instantiate: t -> {args: unit -> Type.t vector,
				   instance: Type.t}
	    val layout: t -> Layout.t
	    val layoutPretty: t -> Layout.t
	    val make: {canGeneralize: bool,
		       ty: Type.t,
		       tyvars: Tyvar.t vector} -> t
	    val ty: t -> Type.t
	 end

      (* close (e, t, ts, r) = {bound, scheme} close type
       * t with respect to environment e, including all the tyvars in ts
       * and ensuring than no tyvar in ts occurs free in e.  bound returns
       * the vector of type variables in t that do not occur in e, which
       * isn't known until all flexible record fields are determined,
       * after unification is complete.
       *)
      val close:
	 Tyvar.t vector * Region.t
	 -> Type.t vector
	 -> {bound: unit -> Tyvar.t vector,
	     schemes: Scheme.t vector}
      val closeTop: Region.t -> unit
      val setOpaqueTyconExpansion: Tycon.t * (Type.t vector -> Type.t) -> unit
      val tyconAdmitsEquality: Tycon.t -> Tycon.AdmitsEquality.t ref
   end

signature INFER_TYPE_ENV = TYPE_ENV
