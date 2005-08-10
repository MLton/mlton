(* Copyright (C) 1999-2001 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature MLTON_CONT =
   sig
      type 'a t

      val callcc: ('a t -> 'a) -> 'a
      val prepend: 'a t * ('b -> 'a) -> 'b t
      val throw: 'a t * 'a -> 'b
      val throw': 'a t * (unit -> 'a) -> 'b
   end
