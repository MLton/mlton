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
      structure Tycon:
	 sig
	    type t
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
      sharing TypeStr.Con = Con
      sharing TypeStr.Kind = EnvTypeStr.Kind
      sharing TypeStr.Name = EnvTypeStr.Name
      sharing TypeStr.Record = EnvTypeStr.Record
      sharing TypeStr.Scheme = Scheme
      sharing TypeStr.Tycon = Tycon
      sharing TypeStr.Type = Type
      sharing TypeStr.Tyvar = EnvTypeStr.Tyvar = Tyvar
      structure Element:
	 sig
	    type interface
	    datatype t =
	       Str of {name: Ast.Strid.t,
		       interface: interface}
	     | Type of {name: Ast.Tycon.t,
			typeStr: EnvTypeStr.t}
	     | Val of {name: Ast.Vid.t,
		       scheme: EnvTypeStr.Scheme.t,
		       status: Status.t}
	 end
      
      type t
      sharing type t = Element.interface
      
      val + : t * t -> t
      val bogus: t
      val cons: TypeStr.Cons.t -> t
      val copy: t -> t (* copy renames all flexible tycons. *)
      val empty: t
      val equals: t * t -> bool
      val excons: TypeStr.Cons.t -> t
      val extendTycon: t * Ast.Tycon.t * TypeStr.t -> t
      val fold: t * 'a * (Element.t * 'a -> 'a) -> 'a
      val layout: t -> Layout.t
      val peekLongtycon: t * Ast.Longtycon.t -> TypeStr.t option
      (* realize makes a copy, and instantiate longtycons *)
      val realize: t * (Ast.Longtycon.t * TypeStr.Kind.t -> EnvTypeStr.t) -> t
      val shapeId: t -> ShapeId.t
      val share: t * Ast.Longstrid.t * Ast.Longstrid.t -> unit
      val shareType: t * Ast.Longtycon.t * Ast.Longtycon.t -> unit
      val strs: {name: Ast.Strid.t, interface: t} vector -> t
      val types: {name: Ast.Tycon.t, typeStr: TypeStr.t} vector -> t
      val vals: {name: Ast.Vid.t,
		 scheme: Scheme.t,
		 status: Status.t} vector -> t
      val wheres: t * (Ast.Longtycon.t * TypeStr.t) vector -> unit
   end
