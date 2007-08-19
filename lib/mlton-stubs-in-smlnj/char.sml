(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Char =
   struct
      open Char
      open OpenInt32

      val toCString =
         fn #"\000" => "\\000"
          | c => toCString c
      val isCntrl = fn c => isCntrl c orelse c >= #"\127"
      val maxOrd = fromInt maxOrd
      val ord = fromInt o ord
      val chr = chr o toInt
   end
