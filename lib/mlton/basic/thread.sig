(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature THREAD =
   sig
      include MLTON_THREAD

      (* generate f returns a function g, that when called runs f until f
       * either completes, in which case g returns NONE, or f calls its argument,
       * in which case g returns what f passes it, and then pauses f.
       *)
      val generate: (('a -> unit) -> unit) -> unit -> 'a option
   end
