(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Date: DATE = 
struct

structure Date = Pervasive.Date
open Date

type t = date

structure Weekday =
   struct
      datatype t = datatype weekday
   end

structure Month =
   struct
      datatype t = datatype month

      val toInt: t -> int =
         fn Jan => 1
          | Feb => 2
          | Mar => 3
          | Apr => 4
          | May => 5
          | Jun => 6
          | Jul => 7
          | Aug => 8
          | Sep => 9
          | Oct => 10
          | Nov => 11
          | Dec => 12
   end

val now = fromTimeLocal o Time.now

val layout = Layout.str o toString

fun fmt(d, s) = Date.fmt s d

fun scan(s, r) = Date.scan r s

end
