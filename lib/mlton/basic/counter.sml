(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Counter: COUNTER =
struct

datatype t = T of int ref

fun new n = T(ref n)

fun reset(T r, n) = r := n

fun tick(T r) = Int.inc r

fun value(T r) = !r

fun next c = value c before tick c

val equals = fn (T r, T r') => r = r'

end
