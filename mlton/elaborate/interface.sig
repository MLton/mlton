(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature INTERFACE_STRUCTS = 
   sig
      structure Ast: AST
      structure EnvTypeStr: TYPE_STR
      sharing Ast.Con = EnvTypeStr.Name
      sharing Ast.SortedRecord = EnvTypeStr.Record
      sharing Ast.Tyvar = EnvTypeStr.Tyvar
   end

signature INTERFACE = 
   sig
      include INTERFACE_STRUCTS

      structure AdmitsEquality: ADMITS_EQUALITY
      structure Tycon:
	 sig
	    type t

	    val admitsEquality: t -> AdmitsEquality.t ref
	    val make: {hasCons: bool} -> t
	 end
      structure Tyvar:
	 sig
	    type t
	 end
      structure Type:
	 sig
	    type t
	       
	    val deEta: t * Tyvar.t vector -> Tycon.t option
	 end
      structure Scheme:
	 sig
	    type t

	    val toEnv: t -> EnvTypeStr.Scheme.t
	 end
      structure Status:
	 sig
	    datatype t = Con | Exn | Var
	       
	    val layout: t -> Layout.t
	    val toString: t -> string
	 end
      structure Con:
	 sig
	    type t
	 end
      sharing Con = EnvTypeStr.Con
      structure Cons:
	 sig
	    datatype t = T of {con: Con.t,
			       name: Ast.Con.t,
			       scheme: Scheme.t} vector

	    val empty: t
	    val layout: t -> Layout.t
	 end
      structure Time:
	 sig
	    type t

	    val tick: unit -> t
	 end
      structure TypeStr:
	 sig
	    include TYPE_STR

	    val fromEnv: EnvTypeStr.t -> t
	    val share:
	       (t * Region.t * (unit -> Layout.t))
	       * (t * Region.t * (unit -> Layout.t))
	       * Time.t
	       -> unit
	    val wheree: t * Region.t * (unit -> Layout.t) * Time.t * t -> unit
	    val toEnv: t -> EnvTypeStr.t
	 end
      sharing TypeStr.AdmitsEquality = AdmitsEquality
      sharing TypeStr.Con = Con
      sharing TypeStr.Kind = EnvTypeStr.Kind
      sharing TypeStr.Name = EnvTypeStr.Name
      sharing TypeStr.Record = EnvTypeStr.Record
      sharing TypeStr.Scheme = Scheme
      sharing TypeStr.Tycon = Tycon
      sharing TypeStr.Type = Type
      sharing TypeStr.Tyvar = EnvTypeStr.Tyvar = Tyvar

      type t
      
      val copy: t -> t (* copy renames all flexible tycons. *)
      val equals: t * t -> bool
      val dest: t -> {strs: (Ast.Strid.t * t) array,
		      types: (Ast.Tycon.t * TypeStr.t) array,
		      vals: (Ast.Vid.t * (Status.t * Scheme.t)) array}
      val empty: t
      val layout: t -> Layout.t
      val lookupLongtycon:
	 t * Ast.Longtycon.t * Region.t * {prefix: Ast.Strid.t list}
	 -> TypeStr.t option
      val new: {strs: (Ast.Strid.t * t) array,
		types: (Ast.Tycon.t * TypeStr.t) array,
		vals: (Ast.Vid.t * (Status.t * Scheme.t)) array} -> t
      val peekStrid: t * Ast.Strid.t -> t option
      datatype 'a peekResult =
	 Found of 'a
       | UndefinedStructure of Ast.Strid.t list
      val peekStrids: t * Ast.Strid.t list -> t peekResult
      val peekTycon: t * Ast.Tycon.t -> TypeStr.t option
      val plist: t -> PropertyList.t
      (* realize makes a copy, and instantiate longtycons *)
      val realize:
	 t * {followStrid: 'a * Ast.Strid.t -> 'a,
	      init: 'a,
	      realizeTycon: ('a * Ast.Tycon.t
			     * TypeStr.AdmitsEquality.t
			     * TypeStr.Kind.t
			     * {hasCons: bool} -> EnvTypeStr.t)}
	 -> t
      val renameTycons: (unit -> unit) ref
      val sameShape: t * t -> bool
      val share: t * Ast.Longstrid.t * t * Ast.Longstrid.t * Time.t -> unit
   end
