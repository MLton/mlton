(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TYPE_OPS_STRUCTS =
   sig
      structure Tycon: TYCON

      type t

      val con: Tycon.t * t vector -> t
      val deConOpt: t -> (Tycon.t * t vector) option
      val layout: t -> Layout.t
   end

signature TYPE_OPS =
   sig
      (* Don't want to include TYPE_OPS_STRUCTS because don't want to propagate
       * the Tycon structure, which will cause duplicate specifications later
       * on.
       *)
      type realSize
      type tycon
      type wordSize
      type t

      val array: t -> t
      val arrow: t * t -> t
      val bool: t
      val con: tycon * t vector -> t
      val cpointer: t
      val deArray: t -> t
      val deArrow: t -> t * t
      val deArrowOpt: t -> (t * t) option
      val deConOpt: t -> (tycon * t vector) option
      val deRef: t -> t
      val deTuple: t -> t vector
      val deTupleOpt: t -> t vector option
      val deVector: t -> t
      val deWeak: t -> t
      val exn: t
      val intInf: t
      val isTuple: t -> bool
      val list: t -> t
      val real: realSize -> t
      val reff: t -> t
      val thread: t
      val tuple: t vector -> t
      val unit: t
      val unitRef: t
      val vector: t -> t
      val weak: t -> t
      val word: wordSize -> t
      val word8: t
      val word8Vector: t
      val word32: t
   end
