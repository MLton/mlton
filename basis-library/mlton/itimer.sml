(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonItimer =
   struct
      structure Prim = PrimitiveFFI.MLton.Itimer

      datatype t = Prof | Real | Virtual

      val signal =
         fn Prof => PosixSignal.prof
          | Real => PosixSignal.alrm
          | Virtual => PosixSignal.vtalrm

      val toInt =
         fn Prof => Prim.PROF
          | Real => Prim.REAL
          | Virtual => Prim.VIRTUAL

      fun set' (t, {interval, value}) =
         let
            fun split t =
               let
                  val q = LargeInt.quot (Time.toMicroseconds t, 1000000)
                  val r = LargeInt.rem (Time.toMicroseconds t, 1000000)
               in
                  (C_Time.fromLargeInt q, C_SUSeconds.fromLargeInt r)
               end
            val (s1, u1) = split interval
            val (s2, u2) = split value
         in
            ignore (Prim.set (toInt t, s1, u1, s2, u2))
         end

      fun set (z as (t, _)) =
         if Primitive.MLton.Profile.isOn
            andalso t = Prof
            then let
                    open PosixError
                 in
                    raiseSys inval
                 end
         else set' z
   end
