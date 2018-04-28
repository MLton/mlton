(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor EuclideanRing(S: EUCLIDEAN_RING_STRUCTS)
   :> EUCLIDEAN_RING where type t = S.t = 
struct

open S

structure IntInf = Pervasive.IntInf

val divMod =
   Trace.traceAssert
   ("EuclideanRing.divMod",
    Layout.tuple2(layout, layout),
    Layout.tuple2(layout, layout),
    fn (p, q) => (not(equals(q, zero)),
                  fn (d, m) => (equals(p, q * d + m)
                                andalso (equals(m, zero)
                                         orelse IntInf.<(metric m, metric q)))))
   divMod

fun p div q = #1(divMod(p, q))

fun p mod q = #2(divMod(p, q))

fun divides(d: t, x: t): bool = equals(x mod d, zero)

val divides =
   Trace.trace("EuclideanRing.divides", Layout.tuple2(layout, layout), Bool.layout) divides

(* Taken from page 812 of CLR. *)
fun extendedEuclidTerm(a: t, b: t, done: t * t -> bool, trace): t * t * t =
   let
      fun loop(a, b) =
         if done(a, b)
            then (a, one, zero)
         else let val (d, m) = divMod(a, b)
                  val (d', x', y') = loop(b, m)
              in (d', y', x' - d * y')
              end
   in trace loop(a, b)
   end

fun makeTraceExtendedEuclid f =
   Trace.traceAssert
   ("EuclideanRing.extendedEuclid",
    Layout.tuple2(layout, layout),
    Layout.tuple3(layout, layout, layout),
    fn (a, b) => (not(isZero a) andalso not(isZero b),
                  fn (d, x, y) => (f(d, x, y)
                                   andalso equals(d, a * x + b * y))))

local
   val trace =
      makeTraceExtendedEuclid
      (fn (d, x, y) => divides(d, x) andalso divides(d, y))
in
   (* Page 72 of Bach and Shallit. *)
   (* Identical to algorithm on page 23 of Berlekamp. *)
   (* This algorithm is slower (about 2x) than the recursive extendedEuclid
    * given above, but stores only a constant number of ring elements.
    * Thus, for now, it is overridden below.
    *)
   fun extendedEuclid(u0: t, u1: t): t * t * t =
      let
         val rec loop =
            fn (r as {m11, m12, m21, m22, u, v, nEven}) =>
            (Assert.assert("EuclideanRing.extendedEuclid", fn () =>
                           equals(u0, m11 * u + m12 * v)
                           andalso equals(u1, m21 * u + m22 * v)
                           andalso equals(if nEven then one else negOne,
                                          m11 * m22 - m12 * m21))
             ; if isZero v
                  then r
               else 
                  let val (q, r) = divMod(u, v)
                  in loop{m11 = q * m11 + m12,
                          m12 = m11,
                          m21 = q * m21 + m22,
                          m22 = m21,
                          u = v,
                          v = r,
                          nEven = not nEven}
                  end)
         val {m12, m22, u, nEven, ...} =
            loop{m11 = one, m12 = zero, m21 = zero, m22 = one,
                 u = u0, v = u1, nEven = true}
         val (a, b) = if nEven then (m22, ~m12) else (~m22, m12)
      in (u, a, b)
      end

   val _ = extendedEuclid

   fun extendedEuclid (a, b) =
      extendedEuclidTerm (a, b, fn (_, b) => equals (b, zero), trace)
end   

local
   val trace = makeTraceExtendedEuclid(fn _ => true)
in
   val extendedEuclidTerm =
      fn (a, b, done) => extendedEuclidTerm(a, b, done, trace)
end

val lastPrime = ref one

fun gcd(a, b) = if isZero b then a else gcd(b, a mod b)

fun lcm(a, b) = (a * b) div gcd(a, b)

val primes: t Stream.t =
   let
      fun loop(s: t Stream.t) =
         Stream.delay
         (fn () => 
          let val (p, s) = valOf(Stream.force s)
             val _ = lastPrime := p
          in Stream.cons
             (p, loop(Stream.keep(s, fn x => not(divides(p, x)))))
          end)
   in loop monics
   end

structure Int =
   struct
      open Pervasive.Int
      type t = int
      val layout = Layout.str o toString
   end

type factors = (t * Int.t) list

fun factor(n: t): factors =
   let
      fun loop(n: t, primes: t Stream.t, factors: factors) =
         if equals(n, one)
            then factors
         else let val (p, primes) = valOf(Stream.force primes)
                  val (n, k) =
                     let
                        fun loop(n, k) =
                           let val (q, r) = divMod(n, p)
                           in if isZero r
                                 then loop(q, Int.+(k, 1))
                              else (n, k)
                           end
                     in loop(n, 0)
                     end
              in loop(n, primes,
                      if k = 0
                         then factors
                      else (p, k) :: factors)
              end
   in loop(n, primes, [])
   end

val factor =
   Trace.traceAssert
   ("EuclideanRing.factor", layout, List.layout (Layout.tuple2(layout, Int.layout)),
    fn n => (not(isZero n), fn factors =>
             equals(n, List.fold(factors, one, fn ((p, k), prod) =>
                                 prod * pow (p, k)))))
   factor

fun existsPrimeOfSmallerMetric(m: IntInf.int, f: t -> bool): bool =
   let
      fun loop primes =
         let val (p, primes) = valOf(Stream.force primes)
         in IntInf.<(metric p, m)
            andalso (f p orelse loop primes)
         end
   in loop primes
   end

fun isPrime(r: t): bool =
   let val r = unitEquivalent r
   in existsPrimeOfSmallerMetric(IntInf.+ (metric r, 1),
                                 fn p => equals(r, p))
   end

fun isComposite(r: t): bool =
   existsPrimeOfSmallerMetric(metric r, fn p => divides(p, r))

end
