(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Buffer: BUFFER = 
struct

datatype 'a t = T of {dummy: 'a,
                      elts: 'a array ref,
                      length: int ref}

fun new {dummy} =
   T {dummy = dummy,
      elts = ref (Array.array (1, dummy)),
      length = ref 0}

fun length (T {length, ...}) = !length

fun reset (T {length, ...}) = length := 0

fun last (T {elts, length = ref n, ...}) =
   if 0 = n
      then NONE
   else SOME (Array.sub (!elts, n - 1))

val growFactor: int = 2

fun ensureFree (T {dummy, elts, length, ...}, amount: int): unit =
   let
      val maxLength = Array.length (!elts)
   in
      if amount <= maxLength - !length
         then ()
      else
         let
            val n = Int.max (maxLength * growFactor, !length + amount)
            val e = !elts
         in
            elts := Array.tabulate (n, fn i =>
                                    if i < maxLength
                                       then Array.sub (e, i)
                                    else dummy)
         end
   end

fun add (v as T {elts, length, ...}, e) =
   (ensureFree (v, 1)
    ; Array.update (!elts, !length, e)
    ; Int.inc length)

fun toVector (T {elts, length, ...}): 'a vector =
   let
      val elts = !elts
   in
      Vector.tabulate (!length, fn i => Array.sub (elts, i))
   end

fun layout layoutElt b = Vector.layout layoutElt (toVector b)

end
