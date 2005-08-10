(* Copyright (C) 2002-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature MLTON_WORLD =
   sig
      val load: string -> 'a
      (* Save the world to resume with the current thread. *)
      datatype status = Clone | Original
      val save: string -> status
      (* Save the world to resume with the given thread. *)
      val saveThread: string * MLtonThread.Runnable.t -> unit
   end
