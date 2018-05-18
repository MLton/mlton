(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                               Pair                                *)
(*-------------------------------------------------------------------*)

functor Pair(structure X: T
             structure Y: T): PAIR =
struct

structure X = X 
structure Y = Y

type t = X.t * Y.t

fun equals((x, y), (x', y')) = X.equals(x, x') andalso Y.equals(y, y')

local open Layout
in fun layout(x, y) =
   paren(seq[X.layout x, str ", ", Y.layout y])
end
(*
fun output((x, y), out) =
   let val print = Out.outputc out
   in (print "(" ;
       X.output(x, out) ;
       print ", " ;
       Y.output(y, out) ;
       print ")")
   end
  *) 
end
