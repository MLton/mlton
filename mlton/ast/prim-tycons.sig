(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature PRIM_TYCONS_STRUCTS =
   sig
      structure IntSize: INT_SIZE
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE

      type t

      val fromString: string -> t
      val equals: t * t -> bool
   end

signature PRIM_TYCONS =
   sig
      structure IntSize: INT_SIZE
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE

      type tycon

      val array: tycon
      val arrow: tycon
      val bool: tycon
      val char: tycon
      val defaultInt: tycon
      val defaultReal: tycon
      val defaultWord: tycon
      val exn: tycon
      val int: IntSize.t -> tycon
      val ints: (tycon * IntSize.t) list
      val intInf: tycon
      val isIntX: tycon -> bool
      val isRealX: tycon -> bool
      val isWordX: tycon -> bool
      val list: tycon
      val pointer: tycon
      val preThread: tycon
      val prims: tycon list
      val real: RealSize.t -> tycon
      val reals: (tycon * RealSize.t) list
      val reff: tycon
      val thread: tycon
      val tuple: tycon
      val vector: tycon
      val weak: tycon
      val word: WordSize.t -> tycon
      val words: (tycon * WordSize.t) list
   end
