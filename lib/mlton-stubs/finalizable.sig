(* Copyright (C) 2003-2003 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature MLTON_FINALIZABLE =
   sig
      type 'a t

      val addFinalizer: 'a t * ('a -> unit) -> unit
      val finalizeBefore: 'a t * 'b t -> unit
      val new: 'a -> 'a t
      val touch: 'a t -> unit
      val withValue: 'a t * ('a -> 'b) -> 'b
   end
