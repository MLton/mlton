(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TYPE_ENV_STRUCTS = 
   sig
      include ATOMS
   end

signature TYPE_ENV = 
   sig
      include TYPE_ENV_STRUCTS

      structure LayoutPretty:
         sig
            type t = Layout.t * ({isChar: bool} * Tycon.BindingStrength.t)
         end

      structure Time:
         sig
            type t

            val now: unit -> t
            val tick: {region: Region.t} -> unit
         end

      structure Type:
         sig
            include TYPE_OPS

            val admitsEquality: t -> bool
            (* can two types be unified?  not side-effecting. *)
            val canUnify: t * t -> bool
            val checkTime:
               t * Time.t * {preError: unit -> unit} ->
               (Layout.t * t * {tycons: Tycon.t list, tyvars: Tyvar.t list}) option
            val deEta: t * Tyvar.t vector -> Tycon.t option
            val deRecord: t -> (Record.Field.t * t) vector
            val explainDoesNotAdmitEquality: t * {layoutPretty: t -> LayoutPretty.t} -> Layout.t
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
            val layoutPrettyAux: t * {expandOpaque: bool, localTyvarNames: bool} -> Layout.t
            val layoutPretty: t -> Layout.t
            val makeHom: {con: Tycon.t * 'a vector -> 'a,
                          expandOpaque: bool,
                          var: Tyvar.t -> 'a} -> {destroy: unit -> unit,
                                                  hom: t -> 'a}
            val makeLayoutPretty:
               {expandOpaque:bool,
                localTyvarNames: bool} -> {destroy: unit -> unit,
                                           layoutPretty: t -> LayoutPretty.t}
            val makeUnify:
               {layoutPretty: t -> LayoutPretty.t,
                preError: unit -> unit} ->
               t * t * {error: Layout.t * Layout.t *
                               {notes: unit -> Layout.t} -> unit} -> unit
            val new: unit -> t
            val record: t SortedRecord.t -> t
            (* make two types identical (recursively).  side-effecting. *)
            val unify:
               t * t * {error: Layout.t * Layout.t *
                               {notes: unit -> Layout.t} -> unit,
                        preError: unit -> unit} -> unit
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
            val dest: t -> Tyvar.t vector * Type.t
            val fresh: t -> Tyvar.t vector * Type.t
            val fromTycon: Tycon.t -> t
            val fromType: Type.t -> t
            val haveFrees: t vector -> bool vector
            val instantiate: t -> {args: unit -> Type.t vector,
                                   instance: Type.t}
            val kind: t -> TyconKind.t
            val layout: t -> Layout.t
            val layoutPrettyAux: 
               t * {expandOpaque: bool, 
                    localTyvarNames: bool} -> Layout.t
            val layoutPretty: t -> Layout.t
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
            val makeLayoutPretty: unit -> {destroy: unit -> unit, layoutPretty: t -> Layout.t}
            val makeLike: t -> t
         end
      sharing type TyvarExt.t = Tyvar.t

      structure TyconExt:
         sig
            type t
            val admitsEquality: Tycon.t -> AdmitsEquality.t ref
            val kind: Tycon.t -> TyconKind.t
            val layoutAppPretty: Tycon.t * LayoutPretty.t vector -> LayoutPretty.t
            val layoutPretty: Tycon.t -> Layout.t
            val make: {admitsEquality: AdmitsEquality.t,
                       defLayoutPretty: string,
                       kind: TyconKind.t,
                       name: string,
                       region: Region.t} -> Tycon.t
            val makeBogus: {name: string,
                            kind: TyconKind.t,
                            region: Region.t option} -> Tycon.t
            val makeLike: Tycon.t -> Tycon.t
            val region: Tycon.t -> Region.t
            val resetLayoutPretty: {unset: Layout.t -> Layout.t} -> unit
            val scopeNew: (unit -> 'a) -> ('a * Tycon.t list)
            val setLayoutPretty: Tycon.t * Layout.t -> unit
            val setOpaqueExpansion: Tycon.t * (Type.t vector -> Type.t) -> unit
         end
      sharing type TyconExt.t = Tycon.t


      val close:
         Tyvar.t vector * {region: Region.t}
         -> ({isExpansive: bool, ty: Type.t, var: 'a} vector
             * {error: 'a * Layout.t * Tyvar.t list -> unit,
                preError: unit -> unit})
         -> {bound: unit -> Tyvar.t vector,
             schemes: Scheme.t vector}
   end
