(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Types =
   struct
      type ('a, 'b) power =
         {layout: 'a -> Layout.t,
          one: 'a,
          times: 'a * 'a -> 'a}
         -> 'a * 'b
         -> 'a   

      type ('a, 'b) simultaneous =
         {layout: 'a -> Layout.t,
          one: 'a,
          times: 'a * 'a -> 'a}
         -> ('a * 'b) list
         -> 'a
   end

structure Power:
   sig
      val power: ('a, Pervasive.Int.int) Types.power
      val powerInf: ('a, Pervasive.IntInf.int) Types.power
      val simultaneous: ('a, Pervasive.Int.int) Types.simultaneous
      val simultaneousInf: ('a, Pervasive.IntInf.int) Types.simultaneous
   end =
struct

open Types

structure Int = Pervasive.Int
structure Array = Pervasive.Array

fun for(a: Int.int, b: Int.int, f: Int.int -> unit) =
   let fun loop i = if i >= b then () else (f i; loop(i + 1))
   in loop a
   end

type 'a exponent =  {isZero: 'a -> bool,
                     divMod: 'a * 'a -> 'a * 'a,
                     two: 'a}

type 'a base = {one: 'a,
                times: 'a * 'a -> 'a,
                layout: 'a -> Layout.t}

fun ('a, 'b) make
   ({isZero, divMod, two}: 'a exponent)
   ({one, times, layout = _}: 'b base) =
   let
      val op * = times
      (* Repeated squaring. *)
      fun power(b: 'b, n: 'a): 'b =
         let
            (* The loop has been carefully unrolled once to avoid overflow when
             * 'a is a fixed size integer.
             *)
            fun loop(c, b, n) =
               (* c * b^2n = b0^n0 *)
               if isZero n then c else next(c, b * b, n)
            and next(c, b, n) =
               (* c * b^n = b0^n0 *)
               let val (d, m) = divMod(n, two)
               in loop(if isZero m then c else c * b, b, d)
               end
         in if isZero n
               then one
            else next(one, b, n)
         end
      (* Based on page 618 of Handbook of Applied Cryptography. *)
      fun simultaneous(ges: ('b * 'a) list): 'b =
         let
            fun twoPowerWord i : Word.t = Word.<<(0w1, Word.fromInt i)
            val twoPower = Word.toInt o twoPowerWord
            fun doit ges =
               let
                  val n = List.length ges
                  val tableSize = twoPower n
                  val table = Array.array(tableSize, one)
                  val _ =
                     List.foreachi
                     (ges, fn (i, (g, _)) =>
                      let val min = twoPower i
                      in for(min, twoPower(i + 1), fn i =>
                             Array.update(table, i,
                                          g * Array.sub(table, i - min)))
                      end)
                  fun loop(ews: ('a * Word.t) list, Gs: 'b list): 'b list =
                     case ews of
                        [] => Gs
                      | _ =>
                           let
                              val (ews, w) =
                                 List.fold
                                 (ews, ([], 0w0: Word.t),
                                  fn ((e, w'), (ews, w)) =>
                                  let
                                     val (e, m) = divMod(e, two)
                                     val ews =
                                        if isZero e then ews else (e, w') :: ews
                                     val w =
                                        if isZero m then w else Word.orb(w', w)
                                  in (ews, w)
                                  end)
                           in loop(ews, Array.sub(table, Word.toInt w) :: Gs)
                           end
                  val ews = List.mapi (ges, fn (i, (_, e)) =>
                                       (e, twoPowerWord i))
                  val Gs = loop (ews, [])
               in List.fold (Gs, one, fn (G, A) => A * A * G)
               end
            val window = 9
            fun split l =
               let
                  fun loop(l, n, ac) =
                     if n <= 0
                        then (rev ac, l)
                     else (case l of
                              [] => (rev ac, [])
                            | x :: l => loop(l, n - 1, x :: ac))
               in loop(l, window, [])
               end
            fun loop(ges: ('b * 'a) list, ac: 'b): 'b =
               case ges of
                  [] => ac
                | [(g, e)] => ac * power(g, e)
                | _ => let val (ges, rest) = split ges
                       in loop(rest, ac * doit ges)
                       end
         in loop(ges, one)
         end
   in {power = power, simultaneous = simultaneous}
   end

val intExp: Int.int exponent =
   {isZero = fn n => n = 0,
    divMod = fn (a, b) => (a div b, a mod b),
    two = 2}

fun power z = #power(make intExp z)
fun simultaneous z = #simultaneous(make intExp z)

val intInfExp =
   let open Pervasive.IntInf
      val zero = fromInt 0
   in {isZero = fn n => n = zero,
       divMod = divMod,
       two = fromInt 2}
   end

fun powerInf z = #power(make intInfExp z)
fun simultaneousInf z = #simultaneous(make intInfExp z)

end
