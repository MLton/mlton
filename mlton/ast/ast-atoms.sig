(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AST_ATOMS_STRUCTS = 
   sig
      structure AdmitsEquality: ADMITS_EQUALITY
      structure CharSize: CHAR_SIZE
      structure IntSize: INT_SIZE
      structure RealSize: REAL_SIZE
      structure Record: RECORD
      structure SortedRecord: RECORD
      structure Symbol: SYMBOL
      structure TyconKind: TYCON_KIND
      structure WordSize: WORD_SIZE
      sharing Record.Field = SortedRecord.Field
      sharing Symbol = Record.Field.Symbol
   end

signature AST_ATOMS =
   sig
      include AST_ATOMS_STRUCTS

      structure Const: AST_CONST

      structure Tyvar:
         sig
            include AST_ID
            val isEquality: t -> bool
         end

      structure Tycon:
         sig
            include AST_ID
            include PRIM_TYCONS sharing type tycon = t
         end

      structure Con:
         sig
            include AST_ID
            include PRIM_CONS
            sharing type con = t
         end

      structure Var: AST_ID

      structure Vid:
         sig
            include AST_ID

            (* conversions to and from variables and constructors *)
            val fromVar: Var.t -> t
            val fromCon: Con.t -> t
            val toVar: t -> Var.t
            val toCon: t -> Con.t

            (* check special ids *)
            val checkRedefineSpecial:
               t * {allowIt: bool,
                    ctxt: unit -> Layout.t,
                    keyword: string} -> unit
            val checkSpecifySpecial:
               t * {allowIt: bool,
                    ctxt: unit -> Layout.t,
                    keyword: string} -> unit
         end

      structure Strid:
         sig
            include AST_ID
            val uArg: string -> t
            val uRes: string -> t
            val uSig: t
            val uStr: t
         end
      structure Sigid: AST_ID
      structure Fctid: AST_ID

      structure Basid: AST_ID

      structure Longtycon:
         sig
            include LONGID
            val arrow: t
         end sharing Longtycon.Id = Tycon

      structure Longcon: LONGID sharing Longcon.Id = Con
      structure Longvar: LONGID sharing Longvar.Id = Var
      structure Longvid:
         sig
            include LONGID

            val toLongcon: t -> Longcon.t
         end sharing Longvid.Id = Vid
      structure Longstrid: LONGID sharing Longstrid.Id = Strid

      sharing Strid = Longtycon.Strid = Longvar.Strid = Longcon.Strid
         = Longvid.Strid = Longstrid.Strid

      sharing Symbol = Basid.Symbol = Con.Symbol = Fctid.Symbol
         = Longcon.Symbol = Longstrid.Symbol = Longtycon.Symbol
         = Longvar.Symbol = Longvid.Symbol = Sigid.Symbol
         = Strid.Symbol = Tycon.Symbol = Tyvar.Symbol = Vid.Symbol
         = Var.Symbol

      structure Type:
         sig
            type t
            datatype node =
               Con of Longtycon.t * t vector
             | Paren of t
             | Record of (Region.t * t) Record.t
             | Var of Tyvar.t

            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val arrow: t * t -> t
            val checkSyntax: t -> unit
            val con: Tycon.t * t vector -> t
            val layout: t -> Layout.t
            val layoutApp: Layout.t * 'a vector * ('a -> Layout.t) -> Layout.t
            val layoutOption: t option -> Layout.t
            val record: (Region.t * t) Record.t -> t
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

            val checkSyntaxDef: t -> unit
            val checkSyntaxSpec: t -> unit
            val empty: t
            val isEmpty: t -> bool
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

            val checkSyntaxDef: t -> unit
            val checkSyntaxSpec: t -> unit
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

            val checkSyntaxDef: t -> unit
            val checkSyntaxSpec: t -> unit
            val layout: t -> Layout.t
         end
      structure ModIdBind:
         sig
            type t
            datatype node = 
               Fct of {lhs: Fctid.t, rhs: Fctid.t} vector
             | Sig of {lhs: Sigid.t, rhs: Sigid.t} vector
             | Str of {lhs: Strid.t, rhs: Strid.t} vector
            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val checkSyntax: t -> unit
            val layout: t -> Layout.t
         end

      val bind: Layout.t * Layout.t -> Layout.t
      val layoutAnds: string * 'a vector * (Layout.t * 'a -> Layout.t)
                      -> Layout.t
      datatype bindStyle =
         OneLine
       | Split of int
      val layoutAndsBind:
         string * string * 'a vector * ('a -> bindStyle * Layout.t * Layout.t)
         -> Layout.t
      val layoutAndsSusp: string * 'a vector * (bool * Layout.t * 'a -> Layout.t)
                          -> (unit -> Layout.t) vector
      val reportDuplicates:
         'a vector * {ctxt: unit -> Layout.t,
                      equals: 'a * 'a -> bool,
                      layout: 'a -> Layout.t,
                      name: string,
                      region: 'a -> Region.t} -> unit
      val reportDuplicateFields:
         (Record.Field.t * (Region.t * 'a)) vector * {ctxt: unit -> Layout.t} -> unit
      val reportDuplicateTyvars:
         Tyvar.t vector * {ctxt: unit -> Layout.t} -> unit
   end
