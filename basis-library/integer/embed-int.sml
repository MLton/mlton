(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature EMBED_INT =
   sig
      eqtype int
      type big

      val fromBigUnsafe: big -> int
      val sizeInBits: Int32.int
      val toBig: int -> big
   end

functor EmbedInt (structure Big: INTEGER_EXTRA
                  structure Small: EMBED_INT where type big = Big.int): INTEGER =
   struct
      structure Small =
         struct
            open Small
            val precision': Int.int = Int32.toInt sizeInBits
         end

      val () = if Int.< (Small.precision', Big.precision') then ()
               else raise Fail "EmbedWord"

      open Small

      val shift = Word.fromInt (Int.- (Big.precision', precision'))

      val extend: Big.int -> Big.int =
         fn i => Big.~>> (Big.<< (i, shift), shift)

      val toBig: Small.int -> Big.int = extend o Small.toBig

      val precision = SOME precision'

      val maxIntBig = Big.>> (Big.fromInt ~1, Word.+ (shift, 0w1))

      val minIntBig = Big.- (Big.~ maxIntBig, Big.fromInt 1)

      val mask = Big.>> (Big.fromInt ~1, shift)

      fun fromBig (i: Big.int): int =
         let
            val i' = Big.andb (i, mask)
         in
            if i = extend i'
               then fromBigUnsafe i'
            else raise Overflow
         end

      val maxInt = SOME (fromBig maxIntBig)

      val minInt = SOME (fromBig minIntBig)

      local
         val make: (Big.int * Big.int -> Big.int) -> (int * int -> int) =
            fn f => fn (x, y) => fromBig (f (toBig x, toBig y))
      in
         val op * = make Big.*
         val op + = make Big.+
         val op - = make Big.-
         val op div = make Big.div
         val op mod = make Big.mod
         val quot = make Big.quot
         val rem = make Big.rem
      end

      local
         val make: (Big.int * Big.int -> 'a) -> (int * int -> 'a) =
            fn f => fn (x, y) => f (toBig x, toBig y)
      in
         val op < = make Big.<
         val op <= = make Big.<=
         val op > = make Big.>
         val op >= = make Big.>=
         val compare = make Big.compare
      end

      val fromInt = fromBig o Big.fromInt

      val toInt = Big.toInt o toBig

      local
         val make: (Big.int -> Big.int) -> (int -> int) =
            fn f => fn x => fromBig (f (toBig x))
      in
         val ~ = make Big.~
         val abs = make Big.abs
      end

      fun fmt r i = Big.fmt r (toBig i)

      val fromLarge = fromBig o Big.fromLarge

      fun fromString s = Option.map fromBig (Big.fromString s)

      fun max (i, j) = if i >= j then i else j

      fun min (i, j) = if i <= j then i else j

      fun scan r reader state =
         Option.map
         (fn (i, state) => (fromBig i, state))
         (Big.scan r reader state)

      val sign = Big.sign o toBig

      fun sameSign (x, y) = sign x = sign y

      val toLarge = Big.toLarge o toBig

      val toString = Big.toString o toBig
   end

functor Embed8 (Small: EMBED_INT where type big = Int8.int): INTEGER =
   EmbedInt (structure Big = Int8
             structure Small = Small)

functor Embed16 (Small: EMBED_INT where type big = Int16.int): INTEGER =
   EmbedInt (structure Big = Int16
             structure Small = Small)

functor Embed32 (Small: EMBED_INT where type big = Int32.int): INTEGER =
   EmbedInt (structure Big = Int32
             structure Small = Small)

structure Int1 = Embed8 (Primitive.Int1)
structure Int2 = Embed8 (Primitive.Int2)
structure Int3 = Embed8 (Primitive.Int3)
structure Int4 = Embed8 (Primitive.Int4)
structure Int5 = Embed8 (Primitive.Int5)
structure Int6 = Embed8 (Primitive.Int6)
structure Int7 = Embed8 (Primitive.Int7)
structure Int9 = Embed16 (Primitive.Int9)
structure Int10 = Embed16 (Primitive.Int10)
structure Int11 = Embed16 (Primitive.Int11)
structure Int12 = Embed16 (Primitive.Int12)
structure Int13 = Embed16 (Primitive.Int13)
structure Int14 = Embed16 (Primitive.Int14)
structure Int15 = Embed16 (Primitive.Int15)
structure Int17 = Embed32 (Primitive.Int17)
structure Int18 = Embed32 (Primitive.Int18)
structure Int19 = Embed32 (Primitive.Int19)
structure Int20 = Embed32 (Primitive.Int20)
structure Int21 = Embed32 (Primitive.Int21)
structure Int22 = Embed32 (Primitive.Int22)
structure Int23 = Embed32 (Primitive.Int23)
structure Int24 = Embed32 (Primitive.Int24)
structure Int25 = Embed32 (Primitive.Int25)
structure Int26 = Embed32 (Primitive.Int26)
structure Int27 = Embed32 (Primitive.Int27)
structure Int28 = Embed32 (Primitive.Int28)
structure Int29 = Embed32 (Primitive.Int29)
structure Int30 = Embed32 (Primitive.Int30)
structure Int31 = Embed32 (Primitive.Int31)
