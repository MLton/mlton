(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_WORLD =
   sig
      datatype status = Clone | Original

      val load: string -> 'a
      (* Save the world to resume with the current thread. *)
      val save: string -> status
      (* Save the world to resume with the given thread. *)
      val saveThread: string * MLtonThread.Runnable.t -> unit
   end
