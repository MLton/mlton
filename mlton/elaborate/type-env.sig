(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
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

      structure Time:
         sig
            type t

            val now: unit -> t
         end

      structure Type:
         sig
            include TYPE_OPS

            val admitsEquality: t -> bool
            (* can two types be unified?  not side-effecting. *)
            val canUnify: t * t -> bool
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
                                           lay: t -> Layout.t * ({isChar: bool}
                                                                 * Tycon.BindingStrength.t)}
            (* minTime (t, time) makes every component of t occur no later than
             * time.  This will display a type error message if time is before
             * the definition time of some component of t.
             *)
            val minTime: t * Time.t -> unit
            val new: unit -> t
            val record: t SortedRecord.t -> t
            (* make two types identical (recursively).  side-effecting. *)
            val unify:
               t * t * {error: Layout.t * Layout.t -> unit,
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
            val bound: t -> Tyvar.t vector
            val dest: t -> Tyvar.t vector * Type.t
            val fromType: Type.t -> t
            val haveFrees: t vector -> bool vector
            val instantiate: t -> {args: unit -> Type.t vector,
                                   instance: Type.t}
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

      val close:
         Tyvar.t vector * {useBeforeDef: Tycon.t -> unit}
         -> {isExpansive: bool, ty: Type.t} vector
         -> {bound: unit -> Tyvar.t vector,
             schemes: Scheme.t vector,
             unable: Tyvar.t vector}
      val generalize: Tyvar.t vector -> unit -> {unable: Tyvar.t vector}
      val initAdmitsEquality: Tycon.t * Tycon.AdmitsEquality.t -> unit
      val setOpaqueTyconExpansion: Tycon.t * (Type.t vector -> Type.t) -> unit
      val tick: {useBeforeDef: Tycon.t -> unit} -> unit
      val tyconAdmitsEquality: Tycon.t -> Tycon.AdmitsEquality.t ref
      val tyconRegion: Tycon.t -> Region.t option ref
   end
