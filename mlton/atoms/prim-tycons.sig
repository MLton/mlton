(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PRIM_TYCONS_SUBSTRUCTS =
   sig
      structure AdmitsEquality: ADMITS_EQUALITY
      structure CharSize: CHAR_SIZE
      structure IntSize: INT_SIZE
      structure Kind: TYCON_KIND
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE
   end

signature PRIM_TYCONS_STRUCTS =
   sig
      include PRIM_TYCONS_SUBSTRUCTS

      type t

      val fromString: string -> t
      val equals: t * t -> bool
      val layout: t -> Layout.t
   end

signature BINDING_STRENGTH =
   sig
      type t

      val unit: t
   end

signature PRIM_TYCONS =
   sig
      include PRIM_TYCONS_SUBSTRUCTS

      structure BindingStrength: BINDING_STRENGTH

      type tycon

      val array: tycon
      val arrow: tycon
      val bool: tycon
      val char: CharSize.t -> tycon
      val cpointer: tycon
      val deCharX: tycon -> CharSize.t
      val defaultChar: unit -> tycon
      val defaultInt: unit -> tycon
      val defaultReal: unit -> tycon
      val defaultWord: unit -> tycon
      val deIntX: tycon -> IntSize.t option
      val deRealX: tycon -> RealSize.t
      val deWordX: tycon -> WordSize.t
      val exn: tycon
      val int: IntSize.t -> tycon
      val ints: (tycon * IntSize.t) vector
      val intInf: tycon
      val isBool: tycon -> bool
      val isCharX: tycon -> bool
      val isCPointer: tycon -> bool
      val isIntX: tycon -> bool
      val isRealX: tycon -> bool
      val isWordX: tycon -> bool
      val layoutApp:
         tycon * (Layout.t * ({isChar: bool} * BindingStrength.t)) vector
         -> Layout.t * ({isChar: bool} * BindingStrength.t)
      val list: tycon
      val prims: {admitsEquality: AdmitsEquality.t,
                  kind: Kind.t,
                  name: string,
                  tycon: tycon} list
      val real: RealSize.t -> tycon
      val reals: (tycon * RealSize.t) vector
      val reff: tycon
      val thread: tycon
      val tuple: tycon
      val vector: tycon
      val weak: tycon
      val word: WordSize.t -> tycon
      val words: (tycon * WordSize.t) vector
   end
