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

      structure ShapeId: UNIQUE_ID
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
      structure TypeStr:
	 sig
	    include TYPE_STR

	    val fromEnv: EnvTypeStr.t -> t
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

      structure Time:
	 sig
	    type t

	    val tick: unit -> t
	 end

      type t
      
      val + : t * t -> t
      val bogus: t
      val cons: TypeStr.Cons.t -> t
      val copy: t -> t (* copy renames all flexible tycons. *)
      val empty: t
      val equals: t * t -> bool
      val excons: TypeStr.Cons.t -> t
      val extendTycon: t * Ast.Tycon.t * TypeStr.t -> t
      val foreach: t * {handleStr: {name: Ast.Strid.t,
				    interface: t} -> unit,
			handleType: {name: Ast.Tycon.t,
				     typeStr: EnvTypeStr.t} -> unit,
			handleVal: {name: Ast.Vid.t,
				    scheme: EnvTypeStr.Scheme.t,
				    status: Status.t} -> unit} -> unit
      val layout: t -> Layout.t
      val lookupLongtycon: t * Ast.Longtycon.t * (TypeStr.t -> unit) -> unit
      val peekLongtycon: t * Ast.Longtycon.t -> TypeStr.t option
      val peekStrid: t * Ast.Strid.t -> t option
      val plist: t -> PropertyList.t
      (* realize makes a copy, and instantiate longtycons *)
      val realize: t * (Ast.Longtycon.t
			* TypeStr.AdmitsEquality.t
			* TypeStr.Kind.t -> EnvTypeStr.t) -> t
      val reportDuplicates: t * Region.t -> unit
      val shapeId: t -> ShapeId.t
      val share: t * Ast.Longstrid.t * Ast.Longstrid.t * Time.t -> unit
      val shareType: t * Ast.Longtycon.t * Ast.Longtycon.t * Time.t -> unit
      val strs: {name: Ast.Strid.t, interface: t} vector -> t
      val types: {name: Ast.Tycon.t, typeStr: TypeStr.t} vector -> t
      val vals: {name: Ast.Vid.t,
		 scheme: Scheme.t,
		 status: Status.t} vector -> t
      val wheres: t * (Ast.Longtycon.t * TypeStr.t) vector * Time.t -> unit
   end
