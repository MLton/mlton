(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature UNBOUNDED_EPHEMERAL_QUEUE =
   sig
      include EPHEMERAL_QUEUE
      
      val empty: unit -> 'a t
   end
