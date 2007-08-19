(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Timer: TIMER =
   struct
      structure SysUsr =
         struct
            datatype t = T of {sys: Time.time, usr: Time.time}

            fun export (T r) = r

            fun (T {sys, usr}) - (T {sys = s', usr = u'}) =
               T {sys = Time.- (sys, s'),
                  usr = Time.- (usr, u')}
         end

      type cpu_timer = {gc: SysUsr.t, self: SysUsr.t}

      fun startCPUTimer (): cpu_timer =
         let
            val {gc = {utime = gcu, stime = gcs, ...},
                 self = {utime = selfu, stime = selfs}, ...} =
               MLtonRusage.rusage ()
         in
            {gc = SysUsr.T {sys = gcs, usr = gcu},
             self = SysUsr.T {sys = selfs, usr = selfu}}
         end

      fun checkCPUTimes {gc, self} =
         let
            val {gc = g', self = s'} = startCPUTimer ()
            val gc = SysUsr.- (g', gc)
            val self = SysUsr.- (s', self)
         in
            {gc = SysUsr.export gc,
             nongc = SysUsr.export (SysUsr.- (self, gc))}
         end

      fun checkCPUTimer timer =
         let
            val {nongc, gc} = checkCPUTimes timer
         in
            {sys = Time.+ (#sys gc, #sys nongc),
             usr = Time.+ (#usr gc, #usr nongc)}
         end

      val totalCPUTimer =
         let
            val t = startCPUTimer ()
         in
            fn () => t
         end

      val checkGCTime = #usr o #gc o checkCPUTimes

      type real_timer = Time.time

      fun startRealTimer (): real_timer = Time.now ()

      fun checkRealTimer (t: real_timer): Time.time =
         Time.- (startRealTimer (), t)

      val totalRealTimer =
         let
            val t = startRealTimer ()
         in
            fn () => t
         end
   end
