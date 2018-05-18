(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature APPEND_REVERSE =
   sig
      structure L: LIST
      structure I: INTEGER
      sharing L.I = I

      type 'a t
      val empty: unit -> 'a t
      val isEmpty: 'a t -> bool
      val length: 'a t -> I.t
      val destruct: 'a t -> ('a * 'a t) option
      val appendReverse: 'a t * 'a L.t -> 'a t
   end
