(* Copyright (C) 2009,2014 Matthew Fluet.
 * Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor WordX (S: WORD_X_STRUCTS): WORD_X = 
struct

open S

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

val toIntInf = value

fun toIntInfX w =
   let
      val v = value w
      val m = modulus (size w)
   in
      if v >= m div 2
         then v - m
      else v
   end

val toInt = IntInf.toInt o toIntInf

fun toString w = concat ["0x", IntInf.format (toIntInf w, StringCvt.HEX)]

val layout = Layout.str o toString

fun zero s = make (0, s)

val hash = IntInf.hash o toIntInf

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
   val lshift = make IntInf.<<
   val >> = make IntInf.~>> (* OK because we know the value is positive. *)
end

fun equals (w, w') = WordSize.equals (size w, size w') andalso value w = value w'

fun fromChar (c: Char.t) = make (Int.toIntInf (Char.toInt c), WordSize.byte)

val fromIntInf = make

fun isAllOnes w = value w = modulus (size w) - 1

fun isOne w = 1 = value w

fun isZero w = 0 = value w

fun isNegOne w = ~1 = toIntInfX w

local
   fun make f (s, sg) = fromIntInf (f (s, sg), s)
in
   val max = make WordSize.max
   val min = make WordSize.min
end

fun allOnes s = max (s, {signed = false})

local
   fun make f (w, sg) = equals (w, f (size w, sg))
in
   val isMax = make max
   val isMin = make min
end

fun notb w = make (IntInf.notb (value w), size w)

fun one s = make (1, s)

fun toIntInfSg (w, {signed}) =
   if signed then toIntInfX w else toIntInf w

fun resize (w, s) = make (toIntInf w, s)

fun resizeX (w, s) = make (toIntInfX w, s)

fun toChar (w: t): char = Char.fromInt (Int.fromIntInf (value w))

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

fun rshift (w, w', {signed}) =
   if signed then ~>> (w, w') else >> (w, w')

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

local
   val make: ((IntInf.t * IntInf.t -> IntInf.t) * string) -> t * t -> t =
      fn (f,name) => fn (w, w') =>
      if WordSize.equals (size w, size w')
         then make (f (value w, value w'), size w)
      else Error.bug (concat ["WordX.", name])
in
   val add = make (IntInf.+, "add")
   val sub = make (IntInf.-, "sub")
   val andb = make (IntInf.andb, "andb")
   val orb = make (IntInf.orb, "orb")
   val xorb = make (IntInf.xorb, "xorb")
end

fun neg w = make (~ (toIntInfX w), size w)

local
   val make: ((IntInf.t * IntInf.t -> IntInf.t) * string) -> t * t * {signed: bool}-> t =
      fn (f,name) => fn (w, w', s) =>
      if WordSize.equals (size w, size w')
         then make (f (toIntInfSg (w, s), toIntInfSg (w', s)), size w)
      else Error.bug (concat ["WordX.", name])
in
   val op div = make (IntInf.div, "div")
   val op mod = make (IntInf.mod, "mod")
   val mul = make (IntInf.*, "mul")
   val quot = make (IntInf.quot, "quot")
   val rem = make (IntInf.rem, "rem")
end

local
   val make: ((IntInf.t * IntInf.t -> 'a) * string) -> t * t * {signed: bool} -> 'a =
      fn (f,name) => fn (w, w', sg) =>
      if WordSize.equals (size w, size w')
         then f (toIntInfSg (w, sg), toIntInfSg (w', sg))
      else Error.bug (concat ["WordX.", name])
in
   val compare = make (IntInf.compare, "compare")
   val lt = make (IntInf.<, "lt")
   val le = make (IntInf.<=, "le")
   val gt = make (IntInf.>, "gt")
   val ge = make (IntInf.>=, "ge")
end

fun layoutSg {signed} = Layout.record [("signed", Bool.layout signed)]

val lt = Trace.trace3 ("WordX.lt", layout, layout, layoutSg, Bool.layout) lt

end
