(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature TYPE_STR_STRUCTS = 
   sig
      structure AdmitsEquality: ADMITS_EQUALITY
      structure Con:
	 sig
	    type t

	    val layout: t -> Layout.t
	    val newNoname: unit -> t
	 end
      structure Kind: TYCON_KIND
      structure Name:
	 sig
	    type t

	    val layout: t -> Layout.t
	 end
      structure Tycon:
	 sig
	    type t

	    val admitsEquality: t -> AdmitsEquality.t ref
	    val arrow: t
	    val equals: t * t -> bool
	    val exn: t
	    val layout: t -> Layout.t
	    val layoutApp:
	       t * (Layout.t * {isChar: bool, needsParen: bool}) vector
	       -> Layout.t * {isChar: bool, needsParen: bool}
	    val make: unit -> t
	 end
      structure Record: RECORD
      structure Tyvar: TYVAR
      structure Type:
	 sig
	    type t

	    val arrow: t * t -> t
	    val bogus: t
	    val con: Tycon.t * t vector -> t
	    val deArrow: t -> t * t
	    val deEta: t * Tyvar.t vector -> Tycon.t option
	    val exn: t
	    val hom: t * {con: Tycon.t * 'a vector -> 'a,
			  record: 'a Record.t -> 'a,
			  var: Tyvar.t -> 'a} -> 'a
	    val layout: t -> Layout.t
	    val record: t Record.t -> t
	    val var: Tyvar.t -> t
	 end
      structure Scheme:
	 sig
	    type t

	    val admitsEquality: t -> bool
	    val apply: t * Type.t vector -> Type.t
	    val bogus: unit -> t
	    val dest: t -> Tyvar.t vector * Type.t
	    val layout: t -> Layout.t
	    val make: Tyvar.t vector * Type.t -> t
	    val ty: t -> Type.t
	 end
   end

signature TYPE_STR = 
   sig
      include TYPE_STR_STRUCTS

      structure Cons:
	 sig
	    datatype t = T of {con: Con.t,
			       name: Name.t,
			       scheme: Scheme.t} vector

	    val empty: t
	    val layout: t -> Layout.t
	 end
      
      type t

      datatype node =
	 Datatype of {cons: Cons.t,
		      tycon: Tycon.t}
       | Scheme of Scheme.t
       | Tycon of Tycon.t

      val abs: t -> t
      val admitsEquality: t -> AdmitsEquality.t
      val apply: t * Type.t vector -> Type.t
      val bogus: Kind.t -> t
      val cons: t -> Cons.t
      val data: Tycon.t * Kind.t * Cons.t -> t
      val def: Scheme.t * Kind.t -> t
      val kind: t -> Kind.t
      val layout: t -> Layout.t
      val node: t -> node
      val toTyconOpt: t -> Tycon.t option (* NONE on Scheme *)
      val tycon: Tycon.t * Kind.t -> t
   end
