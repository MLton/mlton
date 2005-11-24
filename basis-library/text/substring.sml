(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* The :> is to hide the type substring.  We must add the where's to make char
 * and string the same as the toplevel types.
 *)

structure Substring1 
   :> SUBSTRING_EXTRA
   where type char = Char1.char
   where type string = String1.string
   where type substring = Char1VectorSlice.slice
   = SubstringFn(Char1VectorSlice)

structure Substring2
   :> SUBSTRING_EXTRA
   where type char = Char2.char
   where type string = String2.string
   where type substring = Char2VectorSlice.slice
   = SubstringFn(Char2VectorSlice)

structure Substring4
   :> SUBSTRING_EXTRA
   where type char = Char4.char
   where type string = String4.string
   where type substring = Char4VectorSlice.slice
   = SubstringFn(Char4VectorSlice)

structure Substring = Substring1
structure WideSubstring = Substring4

structure SubstringGlobal: SUBSTRING_GLOBAL = Substring
open SubstringGlobal
