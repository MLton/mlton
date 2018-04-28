(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                              Number                               *)
(*-------------------------------------------------------------------*)

functor Number(structure I : INTEGER
               structure R : REAL
               val intToReal : I.t -> R.t) : NUMBER =
struct

structure I = I
structure F = R
structure R = Rational(I)

datatype t =
   Int of I.t
 | Rat of R.t
 | Real of F.t

val intToRat = R.fromInt

fun ratToReal p = F./(intToReal(R.numerator p),
                      intToReal(R.denominator p))

fun toReal(Int m) = intToReal m
  | toReal(Rat p) = ratToReal p
  | toReal(Real x) = x

fun unary(i,r,f) =
   fn Int m => i m
    | Rat p => r p
    | Real x => f x

fun binary(i,r,f) =
   let fun intRat(m,p) = r(p,intToRat m)
      fun intReal(m,x) = f(x,intToReal m)
      fun ratReal(p,x) = f(x,ratToReal p)
   in fn (Int m,Int n) => i(m,n)
       | (Rat p,Rat q) => r(p,q)
       | (Real x,Real y) => f(x,y)
       | (Int m,Rat p) => intRat(m,p)
       | (Rat p,Int m) => intRat(m,p)
       | (Int m,Real x) => intReal(m,x)
       | (Real x,Int m) => intReal(m,x)
       | (Rat p,Real x) => ratReal(p,x)
       | (Real x,Rat p) => ratReal(p,x)
   end

structure OF : BASIC_ORDERED_FIELD =
struct
   type t = t      
   val zero = Int(I.zero)
   val one = Int(I.one)

   fun rat p = if R.isInt p then Int(R.toInt p) else Rat p
   fun close(g,i,r,f) = g(Int o i,rat o r,Real o f)
   val (op +) = close(binary,I.+,R.+,F.+)
   val (op ~) = close(unary,I.~,R.~,F.~)
   val (op * ) = close(binary,I.*,R.*,F.* )
   val inverse = unary(Rat o R.inverse o intToRat,
                       Rat o R.inverse,
                       Real o F.inverse)

   val compare = binary(I.compare,R.compare,F.compare)
   val op < = binary(I.<, R.<, F.<)
   val op = = binary(I.=, R.=, F.=)
   val op <= = binary(I.<=, R.<=, F.<=)
   val op > = binary(I.>, R.>, F.>)
   val op >= = binary(I.>=, R.>=, F.>=)

   fun output(Int m,out) = I.output(m,out)
     | output(Rat p,out) = R.output(p,out)
     | output(Real x,out) = F.output(x,out)
end

structure Field = OrderedField(OF)
open OF Field   

fun isZero z = z = zero
fun isPositive z = z > zero
fun isNegative z = z < zero

(*----------------------------------------*)
(*                Integers                *)
(*----------------------------------------*)

exception UnaryIntOnly
fun unaryIntOnly i =
   fn Int m => i m
    | _ => raise UnaryIntOnly

exception BinaryIntOnly
fun binaryIntOnly i =
   fn (Int m,Int n) => Int(i(m,n))
    | _ => raise BinaryIntOnly

val (op mod) = binaryIntOnly I.mod
val (op quot) = binaryIntOnly I.quot
val (op rem) = binaryIntOnly I.rem
val (op div) = binaryIntOnly I.div
val factorial = unaryIntOnly (Int o I.factorial)
val choose = binaryIntOnly I.choose
val gcd = binaryIntOnly I.gcd
val lcm = binaryIntOnly I.lcm

fun isEven z = isZero(z mod two)
val isOdd = not o isEven

(*----------------------------------------*)
(*               Rationals                *)
(*----------------------------------------*)

exception UnaryRatOnly
fun unaryRatOnly r =
   fn Rat p => r p
    | _ => raise UnaryRatOnly

val numerator = unaryRatOnly R.numerator
val denominator = unaryRatOnly R.denominator

(*----------------------------------------*)
(*                 Reals                  *)
(*----------------------------------------*)

fun toRealUnary f z = Real(f(toReal z))
fun toRealBinary f (z,z')= Real(f(toReal z,toReal z'))

val ln = toRealUnary F.ln
val exp = toRealUnary F.exp
val log = toRealBinary F.log
val log2 = toRealUnary F.log2

val (op ^) = fn (z,z') =>
   if isZero z' then one
   else case (z,z') of
      (Int m,Int n) => if I.isPositive n
                          then Int(I.^(m,n))
                       else Rat(R.inverse
                                (intToRat(I.^(m,I.~ n))))
    | (Int m,_) => Real(F.^(intToReal m,toReal z'))
    | _ => Real(F.^(toReal z,toReal z'))

fun random(Int m,Int n) = Int(I.random(m,n))
  | random(z,z') = Real(F.random(toReal z,toReal z'))

val toReal = F.toReal o toReal
val fromReal = Real o F.fromReal
val toInt = unaryIntOnly I.toInt
val fromInt = Int o I.fromInt

exception Input
fun input _ = raise Input

exception ToString
fun toString _ = raise ToString

exception FromString
fun fromString _ = raise FromString

end

(*
structure Number = Number(structure I = BigInteger
                          structure R = FloatReal
                          val intToReal = I.toReal)
*)
