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
      val deconOpt: t -> (Tycon.t * t vector) option
      val layout: t -> Layout.t
   end

signature TYPE_OPS =
   sig
      (* Don't want to include TYPE_OPS_STRUCTS because don't want to propagate
       * the Tycon structure, which will cause duplicate specifications later
       * on.
       *)
      type intSize
      type realSize
      type tycon
      type wordSize
      type t

      val arg: t -> t    (* arg = #1 o dearrow *)
      val array: t -> t
      val arrow: t * t -> t
      val bool: t
      val con: tycon * t vector -> t
      val dearray: t -> t
      val dearrayOpt: t -> t option
      val dearrow: t -> t * t
      val dearrowOpt: t -> (t * t) option
      val deconOpt: t -> (tycon * t vector) option
      val deconConstOpt: t -> (tycon * tycon vector) option
      val deconConst: t -> (tycon * tycon vector)
      val defaultInt: t
      val defaultReal: t
      val defaultWord: t
      val deref: t -> t
      val derefOpt: t -> t option
      val detuple: t -> t vector
      val detupleOpt: t -> t vector option
      val detycon: t -> tycon
      val devector: t -> t
      val deweak: t -> t
      val deweakOpt: t -> t option
      val exn: t
      val int: intSize -> t
      val intInf: t
      val isTuple: t -> bool
      val list: t -> t
      val nth: t * int -> t
      val pointer: t
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
