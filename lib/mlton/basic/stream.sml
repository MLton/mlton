(* Copyright (C) 2017 Jason Carr
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Stream: STREAM = 
struct

datatype 'a t = T of ('a * 'a t) option Promise.t

fun delayTh th = T (Promise.delay th)
fun force (T p) = Promise.force p

fun 'a delay (th: unit -> 'a t): 'a t = delayTh (force o th)

fun empty () = delayTh (fn () => NONE)

fun cons (x, s) = delayTh (fn () => SOME (x, s))

fun single x = cons (x, empty ())

fun 'a append (s: 'a t, s': 'a t): 'a t =
   let
      fun loop (s) =
         delay (fn () =>
               case force s of
                  NONE => s'
                | SOME (x, s') => cons (x, loop s'))
   in loop s
   end

fun map (s, f) =
   let
      fun loop s =
         delay (fn () =>
               case force s of
                  NONE => empty ()
                | SOME (x, s) => cons (f x, loop s))
   in loop s
   end

fun appendMap (s, f) =
   let
      fun loop (s) =
         delay (fn () =>
               case force s of
                  NONE => empty ()
                | SOME (x, s) => append (f x, loop s))
   in loop s
   end

fun toList (s) =
   case force s of
      NONE => []
    | SOME (x, s) => x :: toList s

fun fromList l =
   case l of
      [] => empty ()
    | x::xs =>
         cons (x, delay (fn () => fromList xs))

fun last (s) =
   let
      fun loop (z, s) =
         case force s of
            NONE => z
          | SOME (x, s) => loop (SOME x, s)
   in loop (NONE, s)
   end

fun isEmpty (s) =
   case force (s) of
      NONE => true
    | SOME _ => false    

fun layout f s = List.layout f (toList s)

fun keep (s, p) =
   let
      fun loop s =
         delay
         (fn () =>
          case force s of
             NONE => empty ()
           | SOME (x, s) => if p x
                              then cons (x, loop s)
                           else loop s)
   in loop s
   end

fun firstN (s, n: int) =
   let
      fun loop (n, s, ac) =
         if n <= 0
            then rev ac
         else (case force s of
                  NONE => Error.bug "Stream.firstN"
                | SOME (x, s) => loop (n - 1, s, x :: ac))
   in loop (n, s, [])
   end

fun firstNSafe (s, n: int) =
   let
      fun loop (n, s, ac) =
         if n <= 0
            then rev ac
         else (case force s of
                  NONE => rev ac
                | SOME (x, s) => loop (n - 1, s, x :: ac))
   in loop (n, s, [])
   end

fun nth (s, n: int) =
   case force s of
      NONE => Error.bug "nth"
    | SOME (x, s) => if n <= 0 then x else nth (s, n - 1)

fun 'a infinite (start: 'a, next: 'a -> 'a): 'a t =
   let fun loop (a: 'a) = delay (fn () => cons (a, loop (next a)))
   in loop start
   end

end
