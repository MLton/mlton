(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_SIGNAL =
   sig
      type t
      type signal = t

      structure Handler:
         sig
            type t

            val default: t
            val handler: (MLtonThread.Runnable.t -> MLtonThread.Runnable.t) -> t
            val ignore: t
            val isDefault: t -> bool
            val isIgnore: t -> bool
            val simple: (unit -> unit) -> t
         end

      structure Mask:
         sig
            type t

            val all: t
            val allBut: signal list -> t
            val block: t -> unit
            val getBlocked: unit -> t
            val isMember: t * signal -> bool
            val none: t
            val setBlocked: t -> unit
            val some: signal list -> t
            val unblock: t -> unit
         end

      val getHandler: t -> Handler.t
      val handled: unit -> Mask.t
      val prof: t
      val restart: bool ref
      val setHandler: t * Handler.t -> unit
      (* suspend m temporarily sets the signal mask to m and suspends until an
       * unmasked signal is received and handled, and then resets the mask.
       *)
      val suspend: Mask.t -> unit
      val vtalrm: t
   end

signature MLTON_SIGNAL_EXTRA =
   sig
      include MLTON_SIGNAL

      val handleGC: (unit -> unit) -> unit
   end
