(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Time: TIME = 
struct

open Pervasive.LargeInt
open Pervasive.Time
structure LargeInt = Pervasive.LargeInt

type t = time

type times =
   {
    self: {utime: t, stime: t},
    children: {utime: t, stime: t},
    gc: {utime: t, stime: t}
    }

fun times (): times =
   let
     val {self, children, gc} = MLton.Rusage.rusage ()
     fun doit ({utime, stime, ...} : MLton.Rusage.t)
       = {utime = utime, stime = stime}
   in
     {self = doit self,
      children = doit children,
      gc = doit gc}
   end

val zero = fromReal 0.0

val equals = op =

val seconds = fromSeconds   

fun minutes m = seconds (m * fromInt 60)

fun hours h = minutes (h * fromInt 60)

fun days d = hours (d * LargeInt.fromInt 24)

fun weeks w = days (w * LargeInt.fromInt 7)

fun years y = days (y * LargeInt.fromInt 365)

val {min, max, ...} = Relation.compare compare

val layout = Layout.str o toString

fun output (t, out) = Out.output (out, toString t)

fun timeThunk (th: unit -> unit): t =
   let
      val {self = {utime, stime}, ...} = times ()
      val t = utime + stime
      val _ = th ()
      val {self = {utime, stime}, ...} = times ()
      val t' = utime + stime
   in t' - t
   end

end
