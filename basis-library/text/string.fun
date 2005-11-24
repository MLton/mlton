(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature STRING0 =
   sig
      structure CharVector : MONO_VECTOR_EXTRA
      structure Char : CHAR
   end

functor StringFn(String0 : STRING0) : STRING_EXTRA =
   struct
      open String0 CharVector
      
      type char = elem
      type string = vector
      val maxSize = maxLen
      val size = length
      fun extract (s, start, len) = 
         CharVectorSlice.vector (CharVectorSlice.slice (s, start, len))
      fun substring (s, start, len) = extract (s, start, SOME len)
      val op ^ = append
      val new = vector
      fun str c = new (1, c)
      val implode = fromList
      val explode = toList
      
      val toLower = translate (str o Char.toLower)

      local
         fun make f = f (op = : char * char -> bool)
      in
        val isPrefix = make isPrefix
        val isSubstring = make isSubvector
        val isSuffix = make isSuffix
      end
      val compare = collate Char.compare
      val {<, <=, >, >=} = Util.makeOrder compare

      val toString = translate Char.toString
      val toCString = translate Char.toCString

      val scan: (char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader =
         fn reader =>
         let
            fun loop (state, cs) =
               case Char.scan reader state of
                  NONE => SOME (implode (rev cs),
                                Char.formatSequences reader state)
                | SOME (c, state) => loop (state, c :: cs)
         in
            fn state => loop (state, [])
         end
         
      val fromString = StringCvt.scanString scan
         
      fun scanString scanChar (reader: (char, 'a) StringCvt.reader)
        : (string, 'a) StringCvt.reader =
         fn state =>
         Option.map (fn (cs, state) => (implode cs, state))
         (Reader.list (scanChar reader) state)

      val fromCString = StringCvt.scanString (scanString Char.scanC)

      fun nullTerm s = s ^ "\000"
   end
