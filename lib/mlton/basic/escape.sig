(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature ESCAPE =
   sig
      type 'a t

      val escape: 'a t * 'a -> 'b
      val new: ('a t -> 'a) -> 'a
   end
