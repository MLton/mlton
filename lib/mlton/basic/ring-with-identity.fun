(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor RingWithIdentity (S: RING_WITH_IDENTITY_STRUCTS)
   :> RING_WITH_IDENTITY where type t = S.t = 
struct

open S

structure IntInf = Pervasive.IntInf

val base = {one = one, layout = layout, times = op *}
val pow = Power.power base
val powInf = Power.powerInf base
val pows = Power.simultaneous base
val powsInf = Power.simultaneousInf base

local
   fun 'a
      make {zero: 'a, < : 'a * 'a -> bool, ~ : 'a -> 'a,
            power: {one: t,
                    layout: t -> Layout.t,
                    times: t * t -> t
                    } -> (t * 'a) -> t}
      (i: 'a) : t =
      let
         val (i, fix) =
            if i < zero
               then (~ i, S.~)
            else (i, fn x => x)
      val i = power{one = S.zero, layout = layout, times = op +} (one, i)
      in fix i
      end
in
   val fromInt = make{zero = 0,
                      < = op <,
                      ~ = Pervasive.Int.~,
                      power = Power.power}
   val fromIntInf = make{zero = 0,
                         < = IntInf.<,
                         ~ = IntInf.~,
                         power = Power.powerInf}
end

(* val fromIntInf =
 *    Trace.trace("fromIntInf", Layout.str o IntInf.toString, layout) fromIntInf
 *)

fun add1 i = i + one

fun sub1 i = i - one

fun inc r = r := add1(!r)

fun dec r = r := sub1(!r)

fun prod l = List.fold(l, one, op * )

val negOne = sub1 zero

val two = add1 one

val three = add1 two

val pows =
   Trace.traceAssert
   ("RingWithIdentity.pows",
    List.layout (Layout.tuple2 (layout, Layout.str o Pervasive.Int.toString)),
    layout,
    fn l => (true, fn r => equals (r, List.fold (l, one, fn ((b, e), ac) =>
                                                 ac * pow (b, e)))))
   pows

val powsInf =
   Trace.traceAssert
   ("RingWithIdentity.powsInf",
    List.layout (Layout.tuple2 (layout, Layout.str o Pervasive.IntInf.toString)),
    layout,
    fn l => (true, fn r => equals (r, List.fold (l, one, fn ((b, e), ac) =>
                                                 ac * powInf (b, e)))))
   powsInf

end
