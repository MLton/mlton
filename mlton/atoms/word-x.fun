functor WordX (S: WORD_X_STRUCTS): WORD_X = 
struct

open S

type int = Int.t
type word = Word.t

val modulus: WordSize.t -> IntInf.t =
   fn s => IntInf.<< (1, Bits.toWord (WordSize.bits s))

local
   datatype t = T of {size: WordSize.t,
		      value: IntInf.t}
in
   type t = t
   fun make (i: IntInf.t, s: WordSize.t) =
      T {size = s,
	 value = i mod modulus s}
   fun dest (T r) = r
end

local
   fun make f = f o dest
in
   val size = make #size
   val value = make #value
end

fun toString w = IntInf.format (value w, StringCvt.HEX)

val layout = Layout.str o toString

fun zero s = make (0, s)

local
   val make: (IntInf.t * Word.t -> IntInf.t) -> t * t -> t =
      fn f => fn (w, w') =>
      let
	 val s = size w
	 val v' = value w'
      in
	 if v' >= Bits.toIntInf (WordSize.bits s)
	    then zero s
	 else make (f (value w, Word.fromIntInf v'), s)
      end
in
   val << = make IntInf.<<
   val >> = make IntInf.~>> (* OK because we know the value is positive. *)
end

fun equals (w, w') = WordSize.equals (size w, size w') andalso value w = value w'

fun fromChar (c: Char.t) = make (Int.toIntInf (Char.toInt c), WordSize.byte)

val fromIntInf = make

fun fromWord8 w = make (Word8.toIntInf w, WordSize.byte)

fun isAllOnes w = value w = modulus (size w) - 1

val isMax = isAllOnes

fun isOne w = 1 = value w

fun isZero w = 0 = value w

fun max s = make (modulus s - 1, s)

fun notb w = make (IntInf.notb (value w), size w)

fun one s = make (1, s)

fun resize (w, s) = make (value w, s)

fun toIntInfX w =
   let
      val v = value w
      val m = modulus (size w)
   in
      if v >= m div 2
	 then v - m
      else v
   end
   
fun resizeX (w, s) = make (toIntInfX w, s)

fun toChar (w: t): char = Char.fromInt (Int.fromIntInf (value w))

val toIntInf = value

fun ~>> (w, w') =
   let
      val shift = value w'
      val s = size w
      val b = WordSize.bits s
      val shift = if shift > Bits.toIntInf b
		     then Bits.toWord b
		  else Word.fromIntInf shift
   in
      make (IntInf.~>> (toIntInfX w, shift), s)
   end

fun swap (i: IntInf.t, {hi: word, lo: word}) =
   let
      open IntInf
   in
      orb (~>> (i, lo), << (i mod << (1, lo), hi))
   end

fun rol (w, w') =
   let
      val s = size w
      val b = WordSize.bits s
      val shift = Word.fromIntInf (value w' mod Bits.toIntInf b)
   in
      make (swap (value w, {hi = shift, lo = Bits.toWord b - shift}), s)
   end

fun ror (w, w') =
   let
      val s = size w
      val b = WordSize.bits s
      val shift = Word.fromIntInf (value w' mod Bits.toIntInf b)
   in
      make (swap (value w, {hi = Bits.toWord b - shift, lo = shift}), s)
   end

fun splice {hi, lo} =
   fromIntInf (value lo
	       + IntInf.<< (value hi, Bits.toWord (WordSize.bits (size lo))),
	       WordSize.+ (size hi, size lo))
   
fun split (w, {lo}) =
   let
      val {size, value} = dest w
      val (q, r) = IntInf.quotRem (value, IntInf.<< (1, Bits.toWord lo))
   in
      {hi = fromIntInf (q, WordSize.fromBits (Bits.- (WordSize.bits size, lo))),
       lo = fromIntInf (r, WordSize.fromBits lo)}
   end

fun bitIsSet (w, i: int) =
   1 = IntInf.rem (IntInf.~>> (value w, Word.fromInt i), 2)

local
   val make: (IntInf.t * IntInf.t -> IntInf.t) -> t * t -> t =
      fn f => fn (w, w') =>
      if WordSize.equals (size w, size w')
	 then make (f (value w, value w'), size w)
      else raise Fail "WordX binary"
in
   val op + = make IntInf.+
   val op - = make IntInf.-
   val op * = make IntInf.*
   val andb = make IntInf.andb
   val op div = make IntInf.div
   val op mod = make IntInf.mod
   val orb = make IntInf.orb
   val xorb = make IntInf.xorb
end

local
   val make: (IntInf.t * IntInf.t -> 'a) -> t * t -> 'a =
      fn f => fn (w, w') =>
      if WordSize.equals (size w, size w')
	 then f (value w, value w')
      else Error.bug "WordX compare"
in
   val op < = make IntInf.<
   val op <= = make IntInf.<=
   val op > = make IntInf.>
   val op >= = make IntInf.>=
end

end
