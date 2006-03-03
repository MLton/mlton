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
structure Substring :> SUBSTRING_EXTRA 
                       where type char = char
                       where type string = string
                       where type substring = CharVectorSlice.slice =
   struct
      open PreString.PreSubstring

      val size = length
      val extract = slice
      fun substring (s, start, len) = extract (s, start, SOME len)
      val string = vector
      val getc = getItem
      fun first ss = Option.map #1 (getItem ss)
      val slice = subslice
      val explode = toList
      local
         fun make f = f (op = : char * char -> bool)
      in
        val isPrefix = make isPrefix
        val isSubstring = make isSubvector
        val isSuffix = make isSuffix
        val position = make position
      end
      val compare = collate Char.compare

(*
      type cs = int
         
      fun reader (T {str, start, size}): (char, cs) Reader.reader =
         fn i => if i >= size
                    then NONE
                 else SOME (String.sub (str, start +? i), i + 1)
                    
      fun 'a scanSubstring
         (f: (char, cs) Reader.reader -> ('a, int) Reader.reader)
         (ss: substring): 'a option =
         case f (reader ss) 0 of
            NONE => NONE
          | SOME (a, _) => SOME a
*)
   end

structure SubstringGlobal: SUBSTRING_GLOBAL = Substring
open SubstringGlobal
