(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Char : CHAR =
   struct
      open Pervasive.Char

      (* SML/NJ doesn't escape #"\000" to three octal digits. *)
      val toCString =
         fn #"\000" => "\\000"
          | c => toCString c
   end
