(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Real (Real: sig
                       include PERVASIVE_REAL
                       val one: real
                       val zero: real
                    end): REAL =
struct

type real = Real.real

structure In = In0

structure R = 
   OrderedRing (structure R = 
                   RingWithIdentity (structure R =
                                        Ring (type t = real
                                              open Real
                                              val layout = Layout.str o toString
                                              val equals = Real.==)
                                     open R Real)
                open R Real
                val {compare, ...} =
                   Relation.lessEqual {< = op <, equals = equals})

structure F = OrderedField (open R Real
                            fun inverse x = one / x)
open F Real
open Math

exception Input 
fun input i =
   case fromString (In.inputToSpace i) of
      SOME x => x
    | NONE => raise Input

local
   open Real
in
   val fromInt = fromInt
   val fromIntInf = fromLargeInt
   val toIntInf = toLargeInt IEEEReal.TO_NEAREST
end

structure Format =
   struct
      open StringCvt
      type t = realfmt

      val exact = EXACT
      val sci = SCI
      val fix = FIX
      val gen = GEN
   end

fun format (x, f) = Real.fmt f x

fun choose(n, k) =
   let
      val k = max (k, n - k)
   in
      prodFromTo {from = add1 k,
                  term = fn i => i,
                  to = n}
      / factorial (n - k)
   end

fun log (base, arg) = ln arg / ln base

val ln2 = ln two

fun log2 x = ln x / ln2

fun realPower(m, n) = exp(n * ln m)

val ceiling = ceil

structure Class =
   struct
      datatype t = datatype IEEEReal.float_class
   end

end

structure Real64 = Real (open Real64
                         val one: real = 1.0
                         val zero: real = 0.0)
structure Real = Real64
structure Real32 = Real (open Real32
                         val one: real = 1.0
                         val zero: real = 0.0)
