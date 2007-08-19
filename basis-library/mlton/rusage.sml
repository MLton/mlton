(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonRusage: MLTON_RUSAGE =
   struct
      structure Prim = PrimitiveFFI.MLton.Rusage

      type t = {utime: Time.time, stime: Time.time}

      fun collect (utimeSec, utimeUsec, stimeSec, stimeUsec) =
         let
            fun toTime (sec, usec) =
               let
                  val time_sec =
                     Time.fromSeconds (C_Time.toLargeInt (sec ()))
                  val time_usec =
                     Time.fromMicroseconds (C_SUSeconds.toLargeInt (usec ()))
               in
                  Time.+ (time_sec, time_usec)
               end
         in
            {stime = toTime (stimeSec, stimeUsec),
             utime = toTime (utimeSec, utimeUsec)}
         end

      val measureGC = MLtonGC.setRusageMeasureGC

      val rusage =
         let 
            val () = measureGC true
         in
            fn () =>
            let
               val () = Prim.getrusage ()
               open Prim
            in
               {children = collect (children_utime_sec, children_utime_usec,
                                    children_stime_sec, children_stime_usec),
                gc = collect (gc_utime_sec, gc_utime_usec,
                              gc_stime_sec, gc_stime_usec),
                self = collect (self_utime_sec, self_utime_usec,
                                self_stime_sec, self_stime_usec)}
            end
         end
   end
