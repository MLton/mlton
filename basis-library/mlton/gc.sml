(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
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

      structure Statistics =
         struct
            local
               fun mk conv prim =
                  fn () => conv (prim gcState)
               val mkSize = mk C_Size.toLargeInt
               val mkUIntmax = mk C_UIntmax.toLargeInt
            in
               val bytesAllocated = mkUIntmax getBytesAllocated
               val lastBytesLive = mkSize getLastBytesLive
               val maxBytesLive = mkSize getMaxBytesLive
               val numCopyingGCs = mkUIntmax getNumCopyingGCs
               val numMarkCompactGCs = mkUIntmax getNumMarkCompactGCs
               val numMinorGCs = mkUIntmax getNumMinorGCs
            end
         end

   end
