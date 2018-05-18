(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature WORD =
   sig
      type t

      val * : t * t -> t 
      val + : t * t -> t 
      val - : t * t -> t 
      val < : t * t -> bool 
      val << : t * word -> t
      val <= : t * t -> bool
      val > : t * t -> bool 
      val >= : t * t -> bool 
      val >> : t * word -> t
      val ~ : t -> t
      val ~>> : t * word -> t
      val andb: t * t -> t 
      val compare: t * t -> order 
      val div: t * t -> t
      val equals: t * t -> bool
      val format: t * StringCvt.radix -> string
      val fromChar: char -> t
      val fromInt: int -> t
      val fromIntInf: Pervasive.IntInf.int -> t
      val fromLarge: Pervasive.LargeWord.word -> t
      val fromWord: word -> t
      val fromString: string -> t option
      val layout: t -> Layout.t
      val max: t * t -> t
      val min: t * t -> t 
      val mod: t * t -> t
      val notb: t -> t
      val nthBitIsSet: t * int -> bool
      val orb: t * t -> t
      val toChar: t -> char
      val toInt: t -> int
      val toIntX: t -> int
      val toIntInf: t -> Pervasive.IntInf.int
      val toIntInfX: t -> Pervasive.IntInf.int
      val toLarge: t -> Pervasive.LargeWord.word
      val toLargeX: t -> Pervasive.LargeWord.word
      val toWord: t -> word
      val toWordX: t -> word
      val toString: t -> string
      val wordSize: int
      val xorb: t * t -> t 
   end
