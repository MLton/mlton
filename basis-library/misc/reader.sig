(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature READER =
   sig
      type ('a, 'b) reader = 'b -> ('a * 'b) option

      (* read as many items as possible (never returns NONE) *)
      val list: ('a, 'b) reader -> ('a list, 'b) reader

      (* never return NONE *)
(*      val tokens: ('a -> bool) -> ('a, 'b) reader -> ('a list list, 'b) reader*)
(*      val fields: ('a -> bool) -> ('a, 'b) reader -> ('a list list, 'b) reader *)
	 
      val map: ('a -> 'c) -> ('a, 'b) reader -> ('c, 'b) reader
      val mapOpt: ('a -> 'c option) -> ('a, 'b) reader -> ('c, 'b) reader
	 
      val ignore: ('a -> bool) -> ('a, 'b) reader -> ('a, 'b) reader

      (* read excatly N items *)
      val readerN: ('a, 'b) reader * int -> ('a list, 'b) reader
      val reader2: ('a, 'b) reader -> ('a * 'a, 'b) reader
      val reader3: ('a, 'b) reader -> ('a * 'a * 'a, 'b) reader
      val reader4: ('a, 'b) reader -> ('a * 'a * 'a * 'a, 'b) reader
   end
