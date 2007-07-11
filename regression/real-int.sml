functor Test (structure Real : REAL) =
struct

datatype z = datatype IEEEReal.float_class
datatype z = datatype IEEEReal.rounding_mode

fun showReal r =
   print (Real.fmt StringCvt.EXACT r)

fun showInt i =
   print (Int.fmt StringCvt.DEC i)

fun showLargeInt li =
   print (LargeInt.fmt StringCvt.DEC li)

fun showMode mode =
   case mode of
      TO_NEAREST => print "TO_NEAREST"
    | TO_NEGINF => print "TO_NEGINF"
    | TO_POSINF => print "TO_POSINF"
    | TO_ZERO => print "TO_ZERO"

datatype 'a res = OK of 'a | OVERFLOW
fun showRes showOk r =
   case r of
      OK x => showOk x
    | OVERFLOW => print "Overflow"
fun wrap th = (OK (th ())) handle Overflow => OVERFLOW

fun checkIntToReal i =
   let
      val li = Int.toLarge i
      val ri = Real.fromInt i
      val rli = Real.fromLargeInt li
   in
      ()
      ; print "["
      ; showInt i
      ; print ","
      ; showLargeInt li
      ; print "] --> ["
      ; showReal ri
      ; print ","
      ; showReal rli
      ; print "] --> "
      ; print (if Real.== (ri, rli) then "OK\n" else "BAD\n")
      ; ()
   end

fun checkRealToInt mode r =
   let
      val i = wrap (fn () => Real.toInt mode r)
      val li = Real.toLargeInt mode r
   in
      ()
      ; print "["
      ; showReal r
      ; print ","
      ; showMode mode
      ; print "] --> ["
      ; showRes showInt i
      ; print ","
      ; showLargeInt li
      ; print "] --> "
      ; print (if wrap (fn () => Int.fromLarge li) = i then "OK\n" else "BAD\n")
      ; ()
   end

val is : Int.int list =
   let
      val op + = Int.+
      val op div = Int.div
      val op * = Int.*
      val ~ = Int.~
      val op - = Int.-
      val minInt = valOf Int.minInt
      val maxInt = valOf Int.maxInt
      val (zero,one,two,three) =
         (Int.fromInt 0, Int.fromInt 1,
          Int.fromInt 2, Int.fromInt 3)
      val one_half = maxInt div two
      val four = two * two
      val one_fourth = maxInt div four
      val eight = four * two
      val one_eighth = maxInt div eight
      val sixteen = eight * two
      val one_sixteenth = maxInt div sixteen
      val thirtytwo = sixteen * two
      val one_thirtysecond = maxInt div thirtytwo
      val sixtyfour = thirtytwo * two
      val one_sixtyfourth = maxInt div sixtyfour
   in
      [minInt, minInt + one, minInt + two, minInt + three,
       minInt + one_sixtyfourth, minInt + one_thirtysecond,
       minInt + one_sixteenth, minInt + one_eighth,
       minInt + one_fourth, minInt + one_half,
       ~three,~two,~one,zero,one,two,three,
       maxInt - one_half, maxInt - one_fourth,
       maxInt - one_eighth, maxInt - one_sixteenth,
       maxInt - one_thirtysecond, maxInt - one_sixtyfourth,
       maxInt - three, maxInt - two, maxInt - one, maxInt]
   end
val () = List.app checkIntToReal is
val rs =
   List.map
   (fn i => let
               val r = Real.fromInt i
               fun make (fold,inf) =
                  fold
                  (fn (_,(r,l)) => let
                                      val r' = Real.nextAfter (r, inf)
                                   in
                                      (r',r'::l)
                                   end)
                  (r,[]) (List.tabulate (64,fn _ => ()))
               val make = fn (fold,inf) => #2 (make (fold,inf))
            in
               (make (foldl,Real.negInf)) @
               [r] @
               (make (foldr,Real.posInf))
            end)
   is
val rs = List.concat rs
val () = List.app (checkRealToInt TO_NEAREST) rs
val () = List.app (checkRealToInt TO_NEGINF) rs
val () = List.app (checkRealToInt TO_POSINF) rs
val () = List.app (checkRealToInt TO_ZERO) rs

end

structure Z = Test (structure Real = Real32)
structure Z = Test (structure Real = Real64)
