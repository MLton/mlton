(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonGC =
   struct
      open Primitive.MLton.GC

      val gcState = Primitive.MLton.GCState.gcState

      val pack : unit -> unit =
         fn () => pack gcState
      val unpack : unit -> unit =
         fn () => unpack gcState

      val setHashConsDuringGC : bool -> unit =
         fn b => setHashConsDuringGC (gcState, b)
      val setMessages : bool -> unit =
         fn b => setMessages (gcState, b)
      val setRusageMeasureGC : bool -> unit =
         fn b => setRusageMeasureGC (gcState, b)
      val setSummary : bool -> unit =
         fn b => setSummary (gcState, b)

   end
