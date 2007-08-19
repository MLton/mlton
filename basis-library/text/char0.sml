(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

local
   structure PreCharX =
      struct
         structure Prim8  = Primitive.Char8
         structure Prim16 = Primitive.Char16
         structure Prim32 = Primitive.Char32
         
         type 'a t = {
            chrUnsafe: int -> 'a,
            ord:       'a -> int,
            minChar:   'a,
            maxChar:   'a,
            numChars:  int
            }
         
         val fChar8 : Prim8.char t = {
            chrUnsafe = Prim8.idFromWord8 o Int.sextdToWord8,
            ord       = Int.zextdFromWord8 o Prim8.idToWord8,
            minChar   = #"\000",
            maxChar   = #"\255",
            numChars  = 256
         }
         val fChar16 : Prim16.char t = {
            chrUnsafe = Prim16.idFromWord16 o Int.sextdToWord16,
            ord       = Int.zextdFromWord16 o Prim16.idToWord16,
            minChar   = #"\000",
            maxChar   = #"\uFFFF",
            numChars  = 65536
            }
         val fChar32 : Prim32.char t = {
            chrUnsafe = Prim32.idFromWord32 o Int.sextdToWord32,
            ord       = Int.zextdFromWord32 o Prim32.idToWord32,
            minChar   = #"\000",
            maxChar   = #"\U0010FFFF",
            numChars  = 1114112 (* 0x110000 *)
         }
      end
in
   structure Char : PRE_CHAR =
      struct
         (* set by config/default/default-charX.sml *)
         open Char
         type string = String.string
         
         local
            structure PCX = Char_ChooseChar(PreCharX)
         in
            val { chrUnsafe, ord, minChar, maxChar, numChars } = PCX.f
         end
      
         fun fromChar x = x
      end
   
   structure WideChar : PRE_CHAR =
      struct
         (* set by config/default/default-widecharX.sml *)
         open WideChar
         type string = WideString.string
         
         local
            structure PCX = WideChar_ChooseChar(PreCharX)
         in
            val { chrUnsafe, ord, minChar, maxChar, numChars } = PCX.f
         end
         
         (* safe b/c WideChar >= Char *)
         val fromChar = chrUnsafe o Char.ord
      end
end
