(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Char1 =
   struct
      open Primitive.Char1
      
      type char = char
      type string = string

      val minChar = #"\000"
      val maxOrd: int = 255
      val maxChar = #"\255"
      
      val toUnicode = Primitive.Char1.toWord32
      val fromUnicode = Primitive.Char1.toWord32
      
      structure CharVector = Char1Vector
end

structure Char2 =
   struct
      open Primitive.Char2
      
      type char = char
      type string = string
      
      val minChar = #"\u0000"
      val maxOrd: int = 65535
      val maxChar = #"\uFFFF"
      
      val toUnicode = Primitive.Char2.toWord32
      val fromUnicode = Primitive.Char2.toWord32
      
      structure CharVector = Char2Vector
   end

structure Char4 =
   struct
      open Primitive.Char2
      
      type char = char
      type string = string
      
      val minChar = #"\U00000000"
      val maxOrd: int = 4294967295
      val maxChar = #"\UFFFFFFFF"
      
      fun toUnicode x = x
      fun fromUnicode x = x
      
      structure CharVector = Char4Vector
   end

structure Char1 : CHAR_EXTRA = CharFn(Char1)
structure Char2 : CHAR_EXTRA = CharFn(Char2)
structure Char4 : CHAR_EXTRA = CharFn(Char4)

structure Char = Char1
structure WideChar = Char4

structure CharGlobal: CHAR_GLOBAL = Char
open CharGlobal
