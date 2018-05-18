(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Cygwin =
   struct
      fun toFullWindowsPath p =
         CUtil.C_String.toString
         (PrimitiveFFI.Cygwin.toFullWindowsPath (NullString.nullTerm p))
   end
