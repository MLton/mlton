(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature EMBED_WORD =
   sig
      eqtype word
      type big

      val fromBigUnsafe: big -> word
      val sizeInBits: Int32.int
      val toBig: word -> big
   end

functor EmbedWord (structure Big: WORD
                   structure Small: EMBED_WORD where type big = Big.word): WORD =
   struct
      structure Small =
         struct
            open Small
            val wordSize: Int.int = Int32.toInt sizeInBits
         end

      val () = if Int.< (Small.wordSize, Big.wordSize) then ()
               else raise Fail "EmbedWord"

      open Small

      fun ones size =
         Big.- (Big.<< (Big.fromLarge 0w1, Word.fromInt size),
                Big.fromLarge 0w1)

      val maxWord = ones wordSize

      fun fromBig (w: Big.word): word =
         fromBigUnsafe (Big.andb (w, maxWord))

      fun fromBigOverflow (w: Big.word): word =
         if Big.<= (w, maxWord)
            then fromBigUnsafe w
         else raise Overflow

      fun highBitIsSet (w: Big.word): bool =
         Big.> (w, ones (Int.- (wordSize, 1)))

      fun toBigX (w: word): Big.word =
         let
            val w = toBig w
         in
            if highBitIsSet w
               then Big.orb (w, Big.notb maxWord)
            else w
         end

      local
         val make: (Big.word * Big.word -> Big.word) -> (word * word -> word) =
            fn f => fn (x, y) => fromBig (f (toBig x, toBig y))
      in
         val op * = make Big.*
         val op + = make Big.+
         val op - = make Big.-
         val andb = make Big.andb
         val op div = make Big.div
         val op mod = make Big.mod
         val orb  = make Big.orb
         val xorb  = make Big.xorb
      end

      local
         val make: ((Big.word * Word.word -> Big.word)
                    -> word * Word.word -> word) =
            fn f => fn (w, w') => fromBig (f (toBig w, w'))
      in
         val >> = make Big.>>
         val << = make Big.<<
      end

      fun ~>> (w, w') = fromBig (Big.~>> (toBigX w, w'))

      local
         val make: (Big.word * Big.word -> 'a) -> (word * word -> 'a) =
            fn f => fn (x, y) => f (toBig x, toBig y)
      in
         val op < = make Big.<
         val op <= = make Big.<=
         val op > = make Big.>
         val op >= = make Big.>=
         val compare = make Big.compare
      end

      local
         val make: (Big.word -> Big.word) -> word -> word =
            fn f => fn w => fromBig (f (toBig w))
      in
         val notb = make Big.notb
      end

      local
         val make: ('a -> Big.word) -> 'a -> word =
            fn f => fn a => fromBig (f a)
      in
         val fromInt = make Big.fromInt
         val fromLarge = make Big.fromLarge
         val fromLargeInt = make Big.fromLargeInt
      end

      local
         val make: (Big.word -> 'a) -> word -> 'a =
            fn f => fn w => f (toBig w)
      in
         val toInt = make Big.toInt
         val toLarge = make Big.toLarge
         val toLargeInt = make Big.toLargeInt
         val toString = make Big.toString
      end

      local
         val make: (Big.word -> 'a) -> word -> 'a =
            fn f => fn w => f (toBigX w)
      in
         val toIntX = make Big.toIntX
         val toLargeIntX = make Big.toLargeIntX
         val toLargeX = make Big.toLargeX
      end

      fun fmt r i = Big.fmt r (toBig i)

      val fromLargeWord = fromLarge

      fun fromString s = Option.map fromBigOverflow (Big.fromString s)

      fun max (w, w') = if w >= w' then w else w'

      fun min (w, w') = if w <= w' then w else w'

      fun scan r reader state =
         Option.map
         (fn (w, state) => (fromBigOverflow w, state))
         (Big.scan r reader state)

      val toLargeWord = toLarge

      val toLargeWordX = toLargeX

      fun ~ w = fromLarge 0w0 - w
   end

functor EmbedWord8 (Small: EMBED_WORD where type big = Word8.word): WORD =
   EmbedWord (structure Big = Word8
              structure Small = Small)

functor EmbedWord16 (Small: EMBED_WORD where type big = Word16.word): WORD =
   EmbedWord (structure Big = Word16
              structure Small = Small)

functor EmbedWord32 (Small: EMBED_WORD where type big = Word32.word): WORD =
   EmbedWord (structure Big = Word32
              structure Small = Small)

structure Word1 = EmbedWord8 (Primitive.Word1)
structure Word2 = EmbedWord8 (Primitive.Word2)
structure Word3 = EmbedWord8 (Primitive.Word3)
structure Word4 = EmbedWord8 (Primitive.Word4)
structure Word5 = EmbedWord8 (Primitive.Word5)
structure Word6 = EmbedWord8 (Primitive.Word6)
structure Word7 = EmbedWord8 (Primitive.Word7)
structure Word9 = EmbedWord16 (Primitive.Word9)
structure Word10 = EmbedWord16 (Primitive.Word10)
structure Word11 = EmbedWord16 (Primitive.Word11)
structure Word12 = EmbedWord16 (Primitive.Word12)
structure Word13 = EmbedWord16 (Primitive.Word13)
structure Word14 = EmbedWord16 (Primitive.Word14)
structure Word15 = EmbedWord16 (Primitive.Word15)
structure Word17 = EmbedWord32 (Primitive.Word17)
structure Word18 = EmbedWord32 (Primitive.Word18)
structure Word19 = EmbedWord32 (Primitive.Word19)
structure Word20 = EmbedWord32 (Primitive.Word20)
structure Word21 = EmbedWord32 (Primitive.Word21)
structure Word22 = EmbedWord32 (Primitive.Word22)
structure Word23 = EmbedWord32 (Primitive.Word23)
structure Word24 = EmbedWord32 (Primitive.Word24)
structure Word25 = EmbedWord32 (Primitive.Word25)
structure Word26 = EmbedWord32 (Primitive.Word26)
structure Word27 = EmbedWord32 (Primitive.Word27)
structure Word28 = EmbedWord32 (Primitive.Word28)
structure Word29 = EmbedWord32 (Primitive.Word29)
structure Word30 = EmbedWord32 (Primitive.Word30)
structure Word31 = EmbedWord32 (Primitive.Word31)
