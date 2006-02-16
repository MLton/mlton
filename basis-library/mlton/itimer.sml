(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonItimer =
   struct
      structure Prim = Primitive.Itimer
         
      datatype t = Prof | Real | Virtual

      val signal =
         fn Prof => PosixPrimitive.Signal.prof
          | Real => PosixPrimitive.Signal.alrm
          | Virtual => PosixPrimitive.Signal.vtalrm

      val toInt =
         fn Prof => Prim.prof
          | Real => Prim.real
          | Virtual => Prim.virtual

      fun set' (t, {interval, value}) =
         let
            fun split t =
               let
                  val (q, r) = IntInf.quotRem (Time.toMicroseconds t, 1000000)
               in
                  (IntInf.toInt q, IntInf.toInt r)
               end
            val (s1, u1) = split interval
            val (s2, u2) = split value
         in
            Prim.set (toInt t, s1, u1, s2, u2)
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
