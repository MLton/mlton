functor WordX (S: WORD_X_STRUCTS): WORD_X = 
struct

open S

structure PWord = Word
structure Word = LargeWord
   
datatype z = datatype WordSize.t
   
(* Words are stored with all zeros for the unused bits. *)
local
   datatype t = T of {size: WordSize.t,
		      word: Word.t}
in
   type t = t
   fun make (w, s) =
      T {size = s,
	 word = Word.andb (w, WordSize.max s)}
   fun dest (T r) = r
end

local
   fun make f = f o dest
in
   val size = make #size
   val word = make #word
end

val toLargeWord = word

fun fromWord8 w = make (Word8.toLarge w, W8)

fun equals (w, w') = dest w = dest w'

fun toString w =
   let
      val {word, ...} = dest w
   in
      concat ["0wx", Word.toString word]
   end

val layout = Layout.str o toString

fun fromChar (c: Char.t) =
   make (Word8.toLarge (Word8.fromChar c), WordSize.W8)

fun signExtend (w: t): Word.t =
   let
      val {size = s, word = w} = dest w
      fun check (w', w'') =
	 if Word.fromWord 0w0 = Word.andb (w, Word.fromWord w')
	    then w
	 else Word.orb (w, Word.xorb (Word.~ (Word.fromWord 0w1),
				      Word.fromWord w''))
   in
      case s of
	 W8 => check (0wx80, 0wxFF)
       | W16 => check (0wx8000, 0wxFFFF)
       | W32 => check (0wx80000000, 0wxFFFFFFFF)
       | W64 => w
   end

fun ~>> (w, w') =
   make (Word.~>> (signExtend w,
		   Word.toWord (word w')),
	 size w)

fun rol (w, w') =
   let
      val {size = s, word = w} = dest w
      val {word = w', ...} = dest w'
      val n = Word.fromInt (WordSize.bits s)
      val w' = Word.mod (w', n)
   in
      make (Word.orb (Word.>> (w, Word.toWord (Word.- (n, w'))),
		      Word.<< (w, Word.toWord w')),
	    s)
   end

fun ror (w, w') =
   let
      val {size = s, word = w} = dest w
      val {word = w', ...} = dest w'
      val n = Word.fromInt (WordSize.bits s)
      val w' = Word.mod (w', n)
   in
      make (Word.orb (Word.>> (w, Word.toWord w'),
		      Word.<< (w, Word.toWord (Word.- (n, w')))),
	    s)
   end

fun resize (w, s) = make (word w, s)

fun resizeX (w, s) = make (signExtend w, s)

fun fromLargeInt (i: IntInf.t, s) = make (Word.fromIntInf i, s)

val toIntInf = Word.toIntInf o word

fun toIntInfX w = Word.toIntInfX (signExtend w)

local
   val make: (Word.t * Word.t -> Word.t) -> t * t -> t =
      fn f => fn (w, w') =>
      let
	 val {size = s, word = w} = dest w
	 val {word = w', ...} = dest w'
      in
	 make (f (w, w'), s)
      end
in
   val op + = make Word.+
   val op - = make Word.-
   val op * = make Word.*
   val andb = make Word.andb
   val op div = make Word.div
   val op mod = make Word.mod
   val orb = make Word.orb
   val xorb = make Word.xorb
end

fun notb w = make (Word.notb (word w), size w)

fun isOne w = Word.fromWord 0w1 = word w
	 
fun isZero w = Word.fromWord 0w0 = word w

fun isAllOnes w = word w = WordSize.allOnes (size w)

fun isMax w = word w = WordSize.max (size w)

fun one s = make (Word.fromWord 0w1, s)
   
fun zero s = make (Word.fromWord 0w0, s)

fun max s = make (WordSize.max s, s)

fun toChar (w: t): char =
   let
      val {word = w, ...} = dest w
   in
      Word.toChar w
   end

val toString = Word.toString o word

local
   fun wrap (f: Word.t * PWord.t -> Word.t) (w: t, w': t): t =
      if Word.> (word w', Word.fromInt (WordSize.bits (size w)))
	 then zero (size w)
      else make (f (word w, Word.toWord (word w')),
		 size w)
in
   val << = wrap Word.<<
   val >> = wrap Word.>>
end

local
   fun make (f: Word.t * Word.t -> 'a): t * t -> 'a =
      fn (w, w') =>
      let
	 val {size = s, word = w} = dest w
	 val {size = s', word = w'} = dest w'
      in
	 if WordSize.equals (s, s')
	    then f (w, w')
	 else Error.bug "WordX binary failure"
      end
in
   val op < = make Word.<
   val op <= = make Word.<=
   val op > = make Word.>
   val op >= = make Word.>=
end

end
