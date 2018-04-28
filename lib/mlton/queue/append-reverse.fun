(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                           AppendReverse                           *)
(*-------------------------------------------------------------------*)

functor LazyAppendReverse(): APPEND_REVERSE =
struct

structure L' = LazyListLength(LazyList)

open L'

structure L = L'

end

functor IncrementalAppendReverse(): APPEND_REVERSE =
struct

structure L' = LazyListLength(LazyList)

open L'

val appendReverse = L'.appendReverseIncremental

structure L = L'

end

functor ExplicitAppendReverse(): APPEND_REVERSE =
struct

val {error, ...} = Error.errors("queue", "append-reverse")

structure L = StrictList
structure I = L.I

datatype 'a t =
   List of 'a L.t
 | Cons of 'a * 'a t ref
 | Rot of 'a t * 'a L.t * 'a L.t

fun empty() = List(L.empty())

fun destruct(List l) = (case L.destruct l of
                           NONE => NONE
                         | SOME(x, l) => SOME(x, List l))
  | destruct(Cons(x, r)) = (force r ; SOME(x, !r))
  | destruct _ = error "destruct"
and force r = (case !r of
                  Rot lra => r := rot lra
                | _ => ())
and rot(l, r, a) =                         
   (case (destruct l, L.destruct r) of
       (NONE, SOME(x, _)) => List(L.cons(x, a))
     | (SOME(x, l), SOME(x', r)) =>
          Cons(x, ref(Rot(l, r, L.cons(x', a))))
     | _ => error "rot")

fun appendReverse(l, r) = rot(l, r, L.empty())

fun isEmpty r = case destruct r of
   NONE => true
 | SOME _ => false

fun length(List l) = L.length l
  | length(Rot(l, r, a)) = length l + L.length r + L.length a
  | length(Cons(x, ref r)) = length r + 1

fun output(r, sep, outElt, out) =
   let val print = Out.outputc out
      fun outputList l = L.output(l, sep, outElt, out)
      fun output(List l) = outputList l
        | output(Rot(l, r, a)) = (print "Rot(" ;
                                output l ;
                                print ", [" ;
                                outputList r ;
                                print "], " ;
                                outputList a ;
                                print ")")
        | output(Cons(x, ref r)) = (outElt(x, out) ;
                                   print sep ;
                                   output r)
   in output r
   end

end
