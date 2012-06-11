
functor Test(structure Real: REAL
             val posZero: Real.real
             val posOne: Real.real
             val size: Int.int) =
struct

open Real

val negZero = ~ posZero
val negOne = ~ posOne

val nan = posInf + negInf

val posNan = copySign (nan, posOne)
val negNan = copySign (nan, negOne)

val _ = print (concat ["Testing Real", Int.toString size, "\n"])

fun test (name, r) =
   let
      val () =
         print (concat ["sign(", name, ") = ",
                        (Int.toString (sign r)) handle Domain => "raise Domain",
                        "\n"])
      val () =
         print (concat ["signBit(", name, ") = ",
                        Bool.toString (signBit r), "\n"])
      val () =
         print (concat ["sign(abs(", name, ")) = ",
                        (Int.toString (sign (abs r))) handle Domain => "raise Domain",
                        "\n"])
      val () =
         print (concat ["signBit(abs(", name, ")) = ",
                        Bool.toString (signBit (abs r)), "\n"])
   in
      ()
   end

val () =
   List.app test [("negNan", negNan),
                  ("negInf", negInf),
                  ("negOne", negOne),
                  ("negZero", negZero),
                  ("posZero", posZero),
                  ("posOne", posOne),
                  ("posInf", posInf),
                  ("posNan", posNan)]
end

structure Z = Test(structure Real = Real32
                   val posZero : Real32.real = 0.0
                   val posOne : Real32.real = 1.0
                   val size = 32)
val () = print "\n"
structure Z = Test(structure Real = Real64
                   val posZero : Real64.real = 0.0
                   val posOne : Real64.real = 1.0
                   val size = 64)
