(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PolyUnorderedSet(): POLY_SET =
struct

structure I = Int
structure L = List

type 'a info = {equal: 'a * 'a -> bool,
                output: 'a * Out.t -> unit}

datatype 'a t = T of 'a List.t * 'a info

fun elts(T(xs, _)) = xs

fun empty info = T([], info)

fun isEmpty s = List.isEmpty(elts s)

fun forall(s, f) = L.forall(elts s, f)
fun exists(s, f) = L.exists(elts s, f)
fun foreach(s, f) = L.foreach(elts s, f)

fun contains(T(elts, {equal, ...}), x) =
   L.exists(elts, fn x' => equal(x, x'))

fun s <= s' = forall(s, fn x => contains(s', x))

fun equal(s, s') = s <= s' andalso s' <= s

fun s >= s' = s' <= s

val equals = equal

fun s < s' = s <= s' andalso exists(s', fn x => not(contains(s, x)))

fun s > s' = s' < s

fun add(s as T(elts, info), x) =
   if contains(s, x) then s
   else T(x :: elts, info)

fun subset(T(elts, info), f) =
   T(L.keepAll(elts, f), info)

fun s1 - s2 = subset(s1, fn x => not(contains(s2, x)))

fun s1 + (s2 as T(x2s, _)) = let val T(x1s, info) = s1 - s2
                            in T(L.append(x1s, x2s), info)
                            end

(*fun union ss = L.foldl(ss, empty, op +)*)

fun intersect(s, s') = subset(s, fn x => contains(s', x))

fun toList(T(xs, _)) = xs

fun remove(T(xs, info as {equal, ...}), x) =
   T(L.remove(xs, fn x' => equal(x, x')),
     info)

fun size(T(xs, _)) = L.length xs
(*
fun output(T(elts, {output, ...}), out) =
   let val print = Outstream.outputc out
   in (print "{" ;
       L.output(", ", output) (elts, out) ;
       print "}")
   end
*)
end

structure PolySet = PolyUnorderedSet()
