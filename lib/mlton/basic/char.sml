(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Char: CHAR =
   struct
      open Char0

      val layout = Layout.str o escapeSML

      val fromInt =
         Trace.trace("Char.fromInt", Layout.str o Int.toString, layout) fromInt

      val isDigit = Trace.trace("Char.isDigit", layout, Bool.layout) isDigit
   end
