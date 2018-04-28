(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ExplicitAppendReverse(): APPEND_REVERSE =
struct

structure L = StrictList
structure I = L.I

datatype 'a t =
   List of 'a L.t
 | Cons of 'a * 'a t ref
 | Rotated of 'a t * 'a L.t * 'a L.t

fun empty() = List(L.empty())

exception Destruct and Rotate
fun destruct(List l) = (case L.destruct l of
                           NONE => NONE
                         | SOME(x, l) => SOME(x, List l))
  | destruct(Cons(x, r)) = (force r ; SOME(x, !r))
  | destruct _ = raise Destruct
and force r = (case !r of
                  Rotated lra => r := rotate lra
                | _ => ())
and rotate(l, r, a) =                      
   (case (destruct l, L.destruct r) of
       (NONE, SOME(x, _)) => List(L.cons(x, a))
     | (SOME(x, l), SOME(x', r)) =>
          Cons(x, ref(Rotated(l, r, L.cons(x', a))))
     | _ => raise Rotate)

fun appendReverse(l, r) = rotate(l, r, L.empty())

fun isEmpty r = case destruct r of
   NONE => true
 | SOME _ => false

fun length(List l) = L.length l
  | length(Rotated(l, r, a)) = length l + L.length r + L.length a
  | length(Cons(x, ref r)) = length r + 1

fun output(r, sep, outElt, out) =
   let val print = Out.outputc out
      fun outputList l = L.output(sep, outElt)(l, out)
      fun output(List l) = outputList l
        | output(Rotated(l, r, a)) = (print "Rotated(" ;
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
