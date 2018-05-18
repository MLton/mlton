(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INTERFACE_STRUCTS = 
   sig
      structure Ast: AST
      structure AdmitsEquality: ADMITS_EQUALITY
      structure Kind: TYCON_KIND
      structure EnvTycon:
         sig
            type t

            val admitsEquality: t -> AdmitsEquality.t
            val arrow: t
            val equals: t * t -> bool
            val exn: t
            val kind: t -> Kind.t
            val layout: t -> Layout.t
            val layoutAppPretty:
               t
               * LayoutPretty.t vector
               * {layoutPretty: t -> Layout.t}
               -> LayoutPretty.t
            val layoutAppPrettyNormal:
               Layout.t
               * LayoutPretty.t vector
               -> LayoutPretty.t
            val tuple: t
         end
      structure EnvTypeStr:
         sig
            type t
            val layout: t -> Layout.t
         end
      structure Tyvar:
         sig
            include ID
            val makeNoname: {equality: bool} -> t
            val makeLayoutPretty:
               unit
               -> {destroy: unit -> unit,
                   layoutPretty: t -> Layout.t,
                   localInit: t vector -> unit}
         end
   end

signature INTERFACE = 
   sig
      include INTERFACE_STRUCTS

      structure FlexibleTycon:
         sig
            type typeStr
            type t

            val admitsEquality: t -> AdmitsEquality.t
            val dest: t -> {admitsEquality: AdmitsEquality.t,
                            hasCons: bool,
                            kind: Kind.t,
                            prettyDefault: string}
            val layout: t -> Layout.t
            val layoutPrettyDefault: t -> Layout.t
            val plist: t -> PropertyList.t
            val new: {admitsEquality: AdmitsEquality.t,
                      hasCons: bool,
                      kind: Kind.t,
                      prettyDefault: string,
                      region: Region.t} -> t
            val realize: t * EnvTypeStr.t -> unit
            datatype realization =
               ETypeStr of EnvTypeStr.t
             | TypeStr of typeStr
            val realization: t -> realization option
            val setAdmitsEquality: t * AdmitsEquality.t -> unit
         end
      structure Tycon:
         sig
            datatype t =
               Flexible of FlexibleTycon.t
             | Rigid of EnvTycon.t
            val admitsEquality: t -> AdmitsEquality.t
         end
      structure Record: RECORD
      sharing Record = Ast.SortedRecord
      structure Type:
         sig
            type t

            val arrow: t * t -> t
            val con: Tycon.t * t vector -> t
            val deArrow: t -> t * t
            val deEta: t * Tyvar.t vector -> Tycon.t option
            val exn: t
            val hom: t * {con: Tycon.t * 'a vector -> 'a,
                          record: 'a Record.t -> 'a,
                          var: Tyvar.t -> 'a} -> 'a
            val layout: t -> Layout.t
            val layoutPretty:
               t * {expand: bool,
                    layoutPrettyEnvTycon: EnvTycon.t -> Layout.t,
                    layoutPrettyFlexTycon: FlexibleTycon.t -> Layout.t,
                    layoutPrettyTyvar: Tyvar.t -> Layout.t}
               -> LayoutPretty.t
            val record: t Record.t -> t
            val var: Tyvar.t -> t
         end
      structure Status:
         sig
            datatype t = Con | Exn | Var

            val layout: t -> Layout.t
            val toString: t -> string
         end
      structure Time:
         sig
            type t

            val tick: unit -> t
         end
      structure Scheme:
         sig
            datatype t = T of {ty: Type.t,
                               tyvars: Tyvar.t vector}

            val admitsEquality: t -> bool
            val fromTycon: Tycon.t -> t
            val make: Tyvar.t vector * Type.t -> t
            val ty: t -> Type.t
         end
      structure Cons:
         sig
            type t
            val dest: t -> {name: Ast.Con.t,
                            scheme: Scheme.t} vector
            val empty: t
            val fromSortedVector: {name: Ast.Con.t,
                                   scheme: Scheme.t} vector -> t
            val fromVector: {name: Ast.Con.t,
                             scheme: Scheme.t} vector -> t
            val layout: t -> Layout.t
            val map: t * ({name: Ast.Con.t,
                           scheme: Scheme.t}
                          -> {scheme: Scheme.t}) -> t
         end
      structure TypeStr:
         sig
            type t

            datatype node =
               Datatype of {tycon: Tycon.t,
                            cons: Cons.t,
                            repl: bool}
             | Scheme of Scheme.t
             | Tycon of {eq: bool,
                         tycon: Tycon.t}

            val abs: t -> t
            val admitsEquality: t -> AdmitsEquality.t
            val apply: t * Type.t vector -> Type.t
            val cons: t -> Cons.t
            val data: Tycon.t * Cons.t * bool -> t
            val def: Scheme.t -> t
            val kind: t -> Kind.t
            val layout: t -> Layout.t
            val node: t -> node
            val pushSpec: t * Region.t -> unit
            val repl: t -> t
            val toTyconOpt: t * {expand: bool} -> Tycon.t option (* NONE on Scheme *)
            val tycon: Tycon.t * bool -> t
            val specs: t * Region.t -> Region.t list

            val share:
               {layoutPrettyEnvTycon: EnvTycon.t -> Layout.t,
                layoutPrettyFlexTycon: FlexibleTycon.t -> Layout.t,
                region: Region.t,
                time: Time.t,
                ty1: {name: unit -> Layout.t,
                      region: Region.t,
                      spec: Region.t,
                      tyStr: t},
                ty2: {name: unit -> Layout.t,
                      region: Region.t,
                      spec: Region.t,
                      tyStr: t}}
               -> unit
            val wheree:
               {layoutPrettyEnvTycon: EnvTycon.t -> Layout.t,
                layoutPrettyFlexTycon: FlexibleTycon.t -> Layout.t,
                realization: t,
                region: Region.t,
                time: Time.t,
                ty: {name: unit -> Layout.t,
                     region: Region.t,
                     spec: Region.t,
                     tyStr: t}}
               -> unit
         end
      sharing type FlexibleTycon.typeStr = TypeStr.t
      structure TyconMap:
         sig
            datatype 'a t = T of {strs: (Ast.Strid.t * 'a t) array,
                                  types: (Ast.Tycon.t * 'a) array}

            val empty: unit -> 'a t
            val layout: ('a -> Layout.t) -> 'a t -> Layout.t
            val peekStrid: 'a t * Ast.Strid.t -> 'a t option
            val peekTycon: 'a t * Ast.Tycon.t ->'a option
         end

      type t

      val copy: t -> t (* copy renames all flexible tycons. *)
      val equals: t * t -> bool
      val dest: t -> {strs: (Ast.Strid.t * t) array,
                      types: (Ast.Tycon.t * TypeStr.t) array,
                      vals: (Ast.Vid.t * (Status.t * Scheme.t)) array}
      val empty: t
      val flexibleTycons: t -> FlexibleTycon.t TyconMap.t
      val layout: t -> Layout.t
      val lookupLongstrid:
         t * Ast.Longstrid.t * Region.t * {prefix: Ast.Strid.t list}
         -> t option
      val lookupLongtycon:
         t * Ast.Longtycon.t * Region.t * {prefix: Ast.Strid.t list}
         -> (Ast.Tycon.t * TypeStr.t) option
      val new: {isClosed: bool,
                original: t option,
                strs: (Ast.Strid.t * t) array,
                types: (Ast.Tycon.t * TypeStr.t) array,
                vals: (Ast.Vid.t * (Status.t * Scheme.t)) array} -> t
      val original: t -> t
      val peekStrid: t * Ast.Strid.t -> t option
      datatype 'a peekResult =
         Found of 'a
       | UndefinedStructure of Ast.Strid.t list
      val peekStrids: t * Ast.Strid.t list -> t peekResult
      val peekTycon: t * Ast.Tycon.t -> (Ast.Tycon.t * TypeStr.t) option
      val plist: t -> PropertyList.t
      val share:
         {layoutPrettyEnvTycon: EnvTycon.t -> Layout.t,
          layoutPrettyFlexTycon: FlexibleTycon.t -> Layout.t,
          I1: t, long1: Ast.Longstrid.t,
          I2: t, long2: Ast.Longstrid.t,
          time: Time.t,
          region: Region.t}
         -> unit
   end
