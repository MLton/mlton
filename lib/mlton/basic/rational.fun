(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                             Rational                              *)
(*-------------------------------------------------------------------*)

functor Rational(I : INTEGER) : RATIONAL =
struct

structure F = 
   struct
      structure I = I
      open I

      datatype t = T of I.t * I.t
      (* always use smallest nonnegative denominator *)

      fun numerator(T(n,_)) = n
      fun denominator(T(_,n)) = n

      fun fromInt n = T(n,I.one)

      (*fun intTo = ITo o I.intTo*)
      (*val toInt = I.toInt o toI*)

      fun isInt q = denominator q = one

      exception ToInt
      fun toInt q = if isInt q then numerator q
                    else raise ToInt

      fun toReal(T(p,q)) = I.toReal p / I.toReal q

      val zero = fromInt I.zero
      val one = fromInt I.one

      fun scale(T(p,q),T(p',q')) =
         let val l = I.lcm(q,q')
         in (p * (l div q'),
             p' * (l div q),
             l)
         end

      val (op +) = fn (x,y) =>
         let val (p,p',l) = scale(x,y)
         in T(p + p',l)
         end

      fun inverse(T(p,q)) = if I.isNegative p then T(I.~ q,I.~ p)
                            else T(q,p)

      val (op ~) = fn T(m,n) => T(~m,n)

      fun reduce(p,q) =
         let val g = I.gcd(p,q)
         in (p div g, q div g)
         end

      fun make(p,q) = T(reduce(p,q))

      fun intIntTo(m,n) = make(I.fromInt m,I.fromInt n)

      fun (T(p,q)) * (T(p',q')) =
         let val (p,q') = reduce(p,q')
            val (p',q) = reduce(p',q)
         in T(I.*(p,p'),I.*(q,q'))
         end

      fun compare(x,y) =
         let val (p,q,_) = scale(x,y)
         in I.compare(p,q)
         end

      val {<,<=,>,>=,equal,min,max} = Relation.compare compare
      val op = = equal

      (*fun random(x,y) =
       let val(p,p',q) = scale(x,y)
   in T(I.random(p,p'),q)
   end
       *)
      exception FromString
      fun stringTo _ = raise FromString
      (*fun toString(T(p,q)) = String.concat[I.toString p,
       "/",
       I.toString q]
       *)
      exception Input
      fun input _ = raise Input

      fun output(p,out) =
         if isInt p then I.output(toInt p,out)
         else (I.output(numerator p,out) ;
               Out.output(out,"/") ;
               I.output(denominator p,out))
   end

structure R = OrderedField(F)
open F R

end
