(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature AST_ATOMS_STRUCTS = 
   sig
      structure Record: RECORD
      structure SortedRecord: RECORD
      structure Symbol: SYMBOL
      structure Tyvar: TYVAR
      sharing Record.Field = SortedRecord.Field
      sharing Symbol = Record.Field.Symbol
   end

signature AST_ATOMS =
   sig
      include AST_ATOMS_STRUCTS

      structure Const: AST_CONST

      structure Tycon:
	 sig
	    include AST_ID
	    include PRIM_TYCONS sharing type tycon = t
	 end
    
      structure Var: AST_ID

      structure Con:
	 sig
	    include AST_ID
	    include PRIM_CONS
	    sharing type con = t

            val ensureRedefine: t -> unit
            val ensureSpecify: t -> unit
	 end

      structure Sigid: AST_ID
      structure Strid: AST_ID
      structure Fctid: AST_ID

      structure Vid:
	 sig
	    include AST_ID

	    (* conversions to and from variables and constructors *)
            val fromVar: Var.t -> t
	    val fromCon: Con.t -> t
	    val toVar: t -> Var.t
	    val toCon: t -> Con.t
	    val toFctid: t -> Fctid.t
	 end

      structure Longtycon:
	 sig
	    include LONGID
            val arrow: t
	    val exn: t
	 end sharing Longtycon.Id = Tycon

      structure Longvar: LONGID sharing Longvar.Id = Var
      structure Longcon: LONGID sharing Longcon.Id = Con
      structure Longstrid: LONGID sharing Longstrid.Id = Strid
      structure Longvid:
	 sig
	    include LONGID

	    val bind: t
	    val cons: t
	    val falsee: t
	    val match: t
	    val nill: t
	    val reff: t
	    val truee: t

	    val fromLongcon: Longcon.t -> t
	    val toLongvar: t -> Longvar.t
	    val toLongcon: t -> Longcon.t
	    val toLongstrid: t -> Longstrid.t
	 end sharing Longvid.Id = Vid

      sharing Strid = Longtycon.Strid = Longvar.Strid = Longcon.Strid
	 = Longvid.Strid = Longstrid.Strid

      sharing Symbol = Con.Symbol = Fctid.Symbol = Longcon.Symbol
	 = Longstrid.Symbol = Longtycon.Symbol = Longvar.Symbol = Longvid.Symbol
	 = Sigid.Symbol = Strid.Symbol = Tycon.Symbol = Vid.Symbol = Var.Symbol

      structure Type:
	 sig
	    type t
	    datatype node =
	       Var of Tyvar.t
	     | Con of Longtycon.t * t vector
	     | Record of t SortedRecord.t

	    include WRAPPED sharing type node' = node
			    sharing type obj = t

	    val arrow: t * t -> t
	    val con: Tycon.t * t vector -> t
	    val exn:  t
	    val layout: t -> Layout.t
	    val layoutApp: Layout.t * 'a vector * ('a -> Layout.t) -> Layout.t
	    val layoutOption: t option -> Layout.t
	    val record: t SortedRecord.t -> t
	    val tuple: t vector -> t
	    val unit: t
	    val var: Tyvar.t -> t
	 end
      structure TypBind:
	 sig
	    type t
	    datatype node = T of {def: Type.t,
				  tycon: Tycon.t,
				  tyvars: Tyvar.t vector} vector
	    include WRAPPED sharing type node' = node
			    sharing type obj = t

            val empty: t
	    val layout: t -> Layout.t
	 end
      structure DatBind:
	 sig
	    type t
	    datatype node =
	       T of {datatypes: {cons: (Con.t * Type.t option) vector,
				 tycon: Tycon.t,
				 tyvars: Tyvar.t vector} vector,
		     withtypes: TypBind.t}
	    include WRAPPED sharing type node' = node
			    sharing type obj = t
	    val layout: string * t -> Layout.t
	 end
      structure DatatypeRhs:
	 sig
	    type t
	    datatype node =
	       DatBind of DatBind.t
	     | Repl of {lhs: Tycon.t, rhs: Longtycon.t}
	    include WRAPPED sharing type node' = node
			    sharing type obj = t
	    val layout: t -> Layout.t
	 end

      val bind: Layout.t * Layout.t -> Layout.t
      val layoutAnds: string * 'a list * (Layout.t * 'a -> Layout.t) -> Layout.t
      datatype bindStyle =
	 OneLine
       | Split of int
      val layoutAndsBind:
	 string * string * 'a list * ('a -> bindStyle * Layout.t * Layout.t)
	 -> Layout.t
   end
