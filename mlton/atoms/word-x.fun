functor WordX (S: WORD_X_STRUCTS): WORD_X = 
struct

open S

datatype z = datatype WordSize.t
   
(* Words are stored with all zeros for the unused bits. *)
local
   datatype t = T of {size: WordSize.t,
		      word: word}
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

val toWord = word

fun fromWord8 w = make (Word8.toWord w, W8)

fun equals (w, w') = dest w = dest w'

fun toString w =
   let
      val {word, ...} = dest w
   in
      concat ["0wx", Word.toString word]
   end

val layout = Layout.str o toString

fun fromChar (c: Char.t) =
   make (Word8.toWord (Word8.fromChar c), WordSize.W8)

fun signExtend (w: t): word =
   let
      val {size = s, word = w} = dest w
   in
      case s of
	 W8 => if 0w0 = Word.andb (w, 0wx80)
		  then w
	       else Word.orb (w, 0wxFFFFFF00)
       | W16 => if 0w0 = Word.andb (w, 0wx8000)
		   then w
		else Word.orb (w, 0wxFFFF0000)
       | W32 => w
   end

fun ~>> (w, w') =
   make (Word.~>> (signExtend w, word w'), size w)

fun rol (w, w') =
   let
      val {size = s, word = w} = dest w
      val {word = w', ...} = dest w'
   in
      make (let
	       open Word
	       val s = Word.fromInt (WordSize.size s)
	       val w' = w' mod s
	    in
	       orb (>> (w, s - w'), << (w, w'))
	    end,
	       s)
   end

fun ror (w, w') =
   let
      val {size = s, word = w} = dest w
      val {word = w', ...} = dest w'
   in
      make (let
	       open Word
	       val s = Word.fromInt (WordSize.size s)
	       val w' = w' mod s
	    in
	       orb (>> (w, w'), << (w, s - w'))
	    end,
	       s)
   end

fun resize (w, s) = make (word w, s)

fun resizeX (w, s) = make (signExtend w, s)

fun fromLargeInt (i: IntInf.t, s) = make (Word.fromIntInf i, s)

val toIntInf = Word.toIntInf o word

fun toIntInfX w = Word.toIntInfX (signExtend w)

local
   val make: (word * word -> word) -> t * t -> t =
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
   val << = make Word.<<
   val >> = make Word.>>
   val andb = make Word.andb
   val op div = make Word.div
   val op mod = make Word.mod
   val orb = make Word.orb
   val xorb = make Word.xorb
end

fun notb w = make (Word.notb (word w), size w)

fun isOne w = 0w1 = word w
	 
fun isZero w = 0w0 = word w

fun isAllOnes w = word w = WordSize.allOnes (size w)

fun isMax w = word w = WordSize.max (size w)

fun one s = make (0w1, s)
   
fun zero s = make (0w0, s)

fun allOnes s = make (WordSize.allOnes s, s)

fun max s = make (WordSize.max s, s)

fun toChar w =
   let
      val {word = w, ...} = dest w
   in
      Word8.toChar (Word8.fromWord w)
   end

val toString = Word.toString o word

local
   fun make (f: word * word -> 'a): t * t -> 'a =
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
   val op < = make (op <)
   val op <= = make (op <=)
   val op > = make (op >)
   val op >= = make (op >=)
end

end
