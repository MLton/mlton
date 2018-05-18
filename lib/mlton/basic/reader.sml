(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Reader: READER =
struct

type ('a, 's) t = 's -> ('a * 's) option

fun char(r, c: char) s =
   case r s of
      NONE => NONE
    | SOME(c', s) => if c = c' then SOME((), s) else NONE

fun or rs s =
   let
      val rec loop =
         fn [] => NONE
          | r :: rs =>
               case r s of
                  NONE => loop rs
                | z => z
   in loop rs
   end

fun all r s =
   let
      fun loop(s, ac) =
         case r s of
            NONE => SOME(rev ac, s)
          | SOME(x, s) => loop(s, x :: ac)
   in loop(s, [])
   end

fun firstN(r, n: Int.t) s =
   let
      fun loop(n, s, ac) =
         if n <= 0
            then SOME(rev ac, s)
         else (case r s of
                  NONE => NONE
                | SOME(x, s) => loop(n - 1, s, x :: ac))
   in loop(n, s, [])
   end

fun mapFail(r, f) s =
   case r s of
      NONE => NONE
    | SOME(a, s) => Option.map(f a, fn b => (b, s))

fun map(r, f) = mapFail(r, SOME o f)

fun seq2(r1, r2) s =
   case r1 s of
      NONE => NONE
    | SOME(x1, s) =>
         case r2 s of
            NONE => NONE
          | SOME(x2, s) => SOME((x1, x2), s)


fun seq3(r1, r2, r3) s =
   case r1 s of
      NONE => NONE
    | SOME(x1, s) =>
         case r2 s of
            NONE => NONE
          | SOME(x2, s) =>
               case r3 s of
                  NONE => NONE
                | SOME(x3, s) => SOME((x1, x2, x3), s)

fun seq4(r1, r2, r3, r4) s =
   case seq3(r1, r2, r3) s of
      NONE => NONE
    | SOME((x1, x2, x3), s) =>
         case r4 s of
            NONE => NONE
          | SOME(x4, s) => SOME((x1, x2, x3, x4), s)

fun seq5(r1, r2, r3, r4, r5) s =
   case seq4(r1, r2, r3, r4) s of
      NONE => NONE
    | SOME((x1, x2, x3, x4), s) =>
         case r5 s of
            NONE => NONE
          | SOME(x5, s) => SOME((x1, x2, x3, x4, x5), s)

fun stringOfLength(r, i: Int.t) s =
   let
      fun loop(i, s, cs) =
         if i <= 0
            then SOME(implode(rev cs), s)
         else (case r s of
                  NONE => NONE
                | SOME(c, s) => loop(i - 1, s, c :: cs))
   in loop(i, s, [])
   end

val info = Trace.info "Reader.readFromString"

fun readFromString(rm, s) =
   let val n: Int.t = String.size s
      fun reader(i: Int.t) =
         if i < n
            then SOME(String.sub(s, i), i + 1)
         else NONE
      val reader =
         Trace.traceInfo
         (info,
          Int.layout,
          fn NONE => Layout.str "NONE"
           | SOME(c, _) => Char.layout c,
          fn _ => (true, fn _ => true))
          reader
   in case rm reader (0: Int.t) of
      NONE => NONE
    | SOME(a, i) => if i = n then SOME a else NONE
   end

end
