(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHAR0 =
   sig
      type t = char

      val < : t * t -> bool
      val <= : t * t -> bool
      val > : t * t -> bool
      val >= : t * t -> bool

      val chr: Pervasive.Int.int -> t
      val compare: t * t -> order
      val dash: t
      val digitToInt: t -> Pervasive.Int.int option
      val dquote: t (* " *)
      val equals: t * t -> bool 
      val escapeC: t -> string
      val escapeSML: t -> string
      val fromCString: string -> t option
      val fromHexDigit: int -> t
      val fromDigit: Pervasive.Int.int -> t
      val fromInt: Pervasive.Int.int -> t
      val fromString: string -> t option
      val fromWord8:  Pervasive.Word8.word -> t
      val isAlpha: t -> bool
      val isAlphaNum: t -> bool
      val isAscii: t -> bool
      val isCntrl: t -> bool
      val isDigit: t -> bool
      val isGraph: t -> bool
      val isHexDigit: t -> bool
      val isLower: t -> bool
      val isPrint: t -> bool
      val isSpace: t -> bool
      val isUpper: t -> bool
      val max: t * t -> t
      val maxChar: t
      val maxOrd: int
      val memoize: (char -> 'a) -> (char -> 'a)
      val min: t * t -> t
      val minChar: t
      val newline: t
      val numChars: int
      val ord: t -> Pervasive.Int.int
      val output: t * TextIO.outstream -> unit
      val pred: t -> t
      val space: t
      val succ: t -> t
      val toHexDigit: t -> int
      val toInt: t -> Pervasive.Int.int
      val toCString: t -> string
      val toLower: t -> t
      (* val toPrintable: t -> string *)
      val toString: t -> string
      val toUpper: t -> t
      val toWord8: t -> Pervasive.Word8.word
   end
