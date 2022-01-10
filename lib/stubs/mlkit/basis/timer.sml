(* Copyright (C) 2022 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Timer =
struct
   open Timer
   val checkCPUTimes = fn (_ : cpu_timer) =>
      {gc = {sys = Time.zeroTime, usr = Time.zeroTime},
       nongc = {sys = Time.zeroTime, usr = Time.zeroTime}}
end
