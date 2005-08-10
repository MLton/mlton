(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
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
