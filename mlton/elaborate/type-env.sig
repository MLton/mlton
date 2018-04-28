(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TYPE_ENV_STRUCTS = 
   sig
      include ATOMS
   end

signature TYPE_ENV = 
   sig
      include TYPE_ENV_STRUCTS

      structure Time:
         sig
            type t

            val now: unit -> t
            val tick: {region: Region.t} -> unit
         end

      structure Type:
         sig
            include TYPE_OPS

            (* can two types be unified?  not side-effecting. *)
            val canUnify: t * t -> bool
            val checkTime:
               t * Time.t * {layoutPrettyTycon: Tycon.t -> Layout.t,
                             layoutPrettyTyvar: Tyvar.t -> Layout.t}
               -> (Layout.t * t * {tycons: Tycon.t list, tyvars: Tyvar.t list}) option
            val copy: t -> t
            val deEta: t * Tyvar.t vector -> Tycon.t option
            val deRecord: t -> (Record.Field.t * t) vector
            val flexRecord: t SortedRecord.t -> t * (unit -> bool)
            val hom: t * {con: Tycon.t * 'a vector -> 'a,
                          expandOpaque: bool,
                          record: 'a SortedRecord.t -> 'a,
                          replaceSynonyms: bool,
                          var: Tyvar.t -> 'a} -> 'a
            val isArrow: t -> bool
            val isBool: t -> bool
            val isCharX: t -> bool
            val isCPointer: t -> bool
            val isInt: t -> bool
            val isUnit: t -> bool
            val isUnknown: t -> bool
            val layout: t -> Layout.t
            val layoutPretty:
               t * {expandOpaque: bool,
                    layoutPrettyTycon: Tycon.t -> Layout.t,
                    layoutPrettyTyvar: Tyvar.t -> Layout.t}
               -> LayoutPretty.t
            val makeHom: {con: Tycon.t * 'a vector -> 'a,
                          expandOpaque: bool,
                          var: Tyvar.t -> 'a} -> {destroy: unit -> unit,
                                                  hom: t -> 'a}
            val makeLayoutPretty:
               {expandOpaque: bool,
                layoutPrettyTycon: Tycon.t -> Layout.t,
                layoutPrettyTyvar: Tyvar.t -> Layout.t}
               -> {destroy: unit -> unit,
                   layoutPretty: t -> LayoutPretty.t}
            val new: unit -> t
            val record: t SortedRecord.t -> t
            (* make two types identical (recursively).  side-effecting. *)
            val unify:
               t * t * {error: Layout.t * Layout.t * {notes: unit -> Layout.t} -> unit,
                        layoutPretty: t -> LayoutPretty.t,
                        layoutPrettyTycon: Tycon.t -> Layout.t,
                        layoutPrettyTyvar: Tyvar.t -> Layout.t}
               -> unit
            val unresolvedChar: unit -> t
            val unresolvedInt: unit -> t
            val unresolvedReal: unit -> t
            val unresolvedString: unit -> t
            val unresolvedWord: unit -> t
            val var: Tyvar.t -> t
         end
(*      sharing type Type.intSize = IntSize.t *)
      sharing type Type.realSize = RealSize.t
      sharing type Type.wordSize = WordSize.t
      sharing type Type.tycon = Tycon.t

      structure Scheme:
         sig
            type t

            val admitsEquality: t -> bool
            val apply: t * Type.t vector -> Type.t
            val checkEquality: t * {layoutPrettyTycon: Tycon.t -> Layout.t} -> Layout.t option
            val dest: t -> Tyvar.t vector * Type.t
            val fresh: t -> Tyvar.t vector * Type.t
            val fromTycon: Tycon.t -> t
            val fromType: Type.t -> t
            val haveUnknowns: t -> bool
            val instantiate: t -> {args: unit -> Type.t vector,
                                   instance: Type.t}
            val kind: t -> TyconKind.t
            val layout: t -> Layout.t
            val make: {canGeneralize: bool,
                       ty: Type.t,
                       tyvars: Tyvar.t vector} -> t
            val ty: t -> Type.t
         end

      structure TyvarExt:
         sig
            type t
            val makeString: string * {equality: bool} -> t
            val makeNoname: {equality: bool} -> t
            val makeLayoutPretty:
               unit ->
               {destroy: unit -> unit,
                layoutPretty: t -> Layout.t,
                localInit: t vector -> unit}
            val makeLayoutPrettyLocal:
               unit ->
               {destroy: unit -> unit,
                layoutPretty: t -> Layout.t,
                reset: unit -> unit}
            val makeLike: t -> t
         end
      sharing type TyvarExt.t = Tyvar.t

      structure TyconExt:
         sig
            type t
            val admitsEquality: t -> AdmitsEquality.t
            val kind: t -> TyconKind.t
            val layoutPrettyDefault: t -> Layout.t
            val make: {admitsEquality: AdmitsEquality.t,
                       kind: TyconKind.t,
                       name: string,
                       prettyDefault: string,
                       region: Region.t} -> t
            val makeBogus: {name: string,
                            kind: TyconKind.t,
                            region: Region.t option} -> t
            val makeLike: t -> t
            val region: t -> Region.t
            val scopeNew: (unit -> 'a) -> ('a * t list)
            val setAdmitsEquality: t * AdmitsEquality.t -> unit
            val setOpaqueExpansion: t * (Type.t vector -> Type.t) -> unit
         end
      sharing type TyconExt.t = Tycon.t

      val close:
         {region: Region.t}
         -> (Tyvar.t vector
             * {isExpansive: bool, ty: Type.t, var: 'a} vector
             * {error: 'a * Layout.t * Tyvar.t list -> unit,
                layoutPrettyTycon: Tycon.t -> Layout.t,
                layoutPrettyTyvar: Tyvar.t -> Layout.t})
         -> {bound: unit -> Tyvar.t vector,
             schemes: Scheme.t vector}
   end
