(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
   
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

      val arg: t -> t    (* arg = #1 o dearrow *)
      val array: t -> t
      val arrow: t * t -> t
      val bool: t
      val con: tycon * t vector -> t
      val deArray: t -> t
      val deArrayOpt: t -> t option
      val deArrow: t -> t * t
      val deArrowOpt: t -> (t * t) option
      val deConOpt: t -> (tycon * t vector) option
      val deConConstOpt: t -> (tycon * tycon vector) option
      val deConConst: t -> (tycon * tycon vector)
      val deRef: t -> t
      val deRefOpt: t -> t option
      val deTuple: t -> t vector
      val deTupleOpt: t -> t vector option
      val deTycon: t -> tycon
      val deVector: t -> t
      val deWeak: t -> t
      val deWeakOpt: t -> t option
      val defaultReal: t
      val defaultWord: t
      val exn: t
      val intInf: t
      val isTuple: t -> bool
      val list: t -> t
      val nth: t * int -> t
      val preThread: t
      val real: realSize -> t
      val reff: t -> t
      val result: t -> t (* result = #2 o dearrow *)
      val thread: t
      val tuple: t vector -> t
      val unit: t
      val unitRef: t
      val vector: t -> t
      val weak: t -> t
      val word: wordSize -> t
      val word8: t
      val word8Vector: t
   end
