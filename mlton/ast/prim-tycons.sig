(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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

signature PRIM_TYCONS =
   sig
      include PRIM_TYCONS_SUBSTRUCTS
     
      type tycon

      val array: tycon
      val arrow: tycon
      val bool: tycon
      val char: CharSize.t -> tycon
      val defaultChar: tycon
      val defaultInt: tycon
      val defaultReal: tycon
      val defaultWord: tycon
      val exn: tycon
      val int: IntSize.t -> tycon
      val ints: (tycon * IntSize.t) vector
      val intInf: tycon
      val isCharX: tycon -> bool
      val isIntX: tycon -> bool
      val isRealX: tycon -> bool
      val isWordX: tycon -> bool
      val layoutApp:
	 tycon * (Layout.t * {isChar: bool, needsParen: bool}) vector
	 -> Layout.t * {isChar: bool, needsParen: bool}
      val list: tycon
      val prims: (tycon * Kind.t * AdmitsEquality.t) list
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
