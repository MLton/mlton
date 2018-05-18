(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHAR =
   sig
      type t = char

      val < : t * t -> bool
      val <= : t * t -> bool
      val > : t * t -> bool
      val >= : t * t -> bool
      val chr: int -> t
      val compare: t * t -> Relation.t
      val dash: t
      val digitToInt: t -> int option
      val dquote: t (* " *)
      val equals: t * t -> bool 
      val escapeC: t -> string
      val escapeSML: t -> string
      val fromCString: string -> t option
      val fromDigit: int -> t
      val fromHexDigit: int -> t
      val fromInt: int -> t
      val fromString: string -> t option
      val fromWord8:  Word8.t -> t
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
      val layout: t -> Layout.t
      val max: t * t -> t
      val maxChar: t
      val maxOrd: int
      val memoize: (char -> 'a) -> (char -> 'a)
      val min: t * t -> t
      val minChar: t
      val newline: t
      val numChars: int
      val ord: t -> int
      val output: t * TextIO.outstream -> unit
      val pred: t -> t
      val space: t
      val succ: t -> t
      val toHexDigit: t -> int
      val toInt: t -> int
      val toLower: t -> t
      val toString: t -> string
      val toUpper: t -> t
      val toWord8: t -> Word8.t
   end

functor TestChar (S: CHAR): sig end =
   struct
      open S
      val _ =
         Assert.assert
         ("TestChar", fn () =>
          "\\000" = escapeC #"\000")
   end
