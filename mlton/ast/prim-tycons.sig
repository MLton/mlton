(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature PRIM_TYCONS_STRUCTS =
   sig
      type t
      val fromString: string -> t
      val equals: t * t -> bool
   end

signature PRIM_TYCONS =
   sig
      type tycon

      val array: tycon
      val arrow: tycon
      val bool: tycon
      val char: tycon
      val exn: tycon
      val int: tycon
      val intInf: tycon
      val list: tycon
      val pointer: tycon
      val preThread: tycon
      val real: tycon
      val reff: tycon
      val thread: tycon
      val tuple: tycon
      val vector: tycon
      val weak: tycon
      val word: tycon
      val word8: tycon

      val prims: tycon list

      val defaultInt: tycon
      val defaultWord: tycon
	 
      val isWordX: tycon -> bool
      val isIntX: tycon -> bool
   end
