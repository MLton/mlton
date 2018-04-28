(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure String : STRING =
   struct
      open Pervasive.String

      (* SML/NJ doesn't escape #"\000" to three octal digits. *)
      val toCString = translate Char.toCString
   end
