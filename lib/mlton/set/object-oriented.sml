(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Set () =
struct

type 'a obj = {rep: 'a, equal: 'a * 'a -> bool}

datatype 'a t =
   Empty
 | NonEmpty of {elts: 'a list,
                equal: ('a * 'a -> bool)}

val empty = Empty

fun singleton {rep, equal} = NonEmpty{elts = [rep], equal = equal}

fun add(Empty, re) = singleton re
  | add(NonEmpty{elts, equal}, {rep, equal})


   how do you make sure that it's the same equality function??
