(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure String: STRING_EXTRA =
   struct
      open String0

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

structure StringGlobal: STRING_GLOBAL = String
open StringGlobal

(* Now that concat is defined, we can add the exnMessager for Fail. *)
val _ =
   General.addExnMessager
   (fn e =>
    case e of
       Fail s => SOME (concat ["Fail: ", s])
     | _ => NONE)

structure NullString =
   struct
      open NullString

      val nullTerm = fromString o String.nullTerm
   end
