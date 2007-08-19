(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature STRING_ARG =
   sig
      structure Char: CHAR_EXTRA
      structure CharVector: EQTYPE_MONO_VECTOR_EXTRA
      sharing type Char.char   = CharVector.elem
      sharing type Char.string = CharVector.vector
   end

functor StringFn(Arg : STRING_ARG) 
        :> STRING_EXTRA
             where type char   = Arg.CharVector.elem
             where type string = Arg.CharVector.vector 
             where type array  = Arg.CharVector.array =
   struct
      open Arg
      open CharVector
      structure CharVectorSlice = MonoVectorSlice
      
      type char = elem
      type string = vector
      
      val new = vector
      fun str c = new (1, c)
      
      val maxSize = maxLen
      val size = length
      val op ^ = append
      val implode = fromList
      val explode = toList
      
      fun extract (s, start, len) = 
         CharVectorSlice.vector (CharVectorSlice.slice (s, start, len))
      fun substring (s, start, len) = extract (s, start, SOME len)

      val toLower = translate (str o Char.toLower)

      local
         fun make f = f (op = : char * char -> bool)
      in
        val isPrefix = make isPrefix
        val isSubstring = make isSubvector
        val isSuffix = make isSuffix
      end
      val compare = collate Char.compare
      local
         structure S = StringComparisons (type t = string
                                          val compare = compare)
      in
         open S
      end
      
      fun Stranslate f = String.fromPoly o Vector.translate f o toPoly

      val toString = Stranslate Char.toString
      val toCString = Stranslate Char.toCString

      val scan =
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

      fun scanString scanChar reader =
         fn state =>
         Option.map (fn (cs, state) => (implode cs, state))
         (Reader.list (scanChar reader) state)

      val fromCString = StringCvt.scanString (scanString Char.scanC)

      val null = str (Char.chr 0)
      fun nullTerm s = s ^ null
   end

structure StringArg : STRING_ARG =
   struct
      structure Char = Char
      structure CharVector = CharVector
   end

structure WideStringArg : STRING_ARG =
   struct
      structure Char = WideChar
      structure CharVector = WideCharVector
   end

structure String : STRING_EXTRA = StringFn(StringArg)
structure WideString : STRING_EXTRA = StringFn(WideStringArg)
