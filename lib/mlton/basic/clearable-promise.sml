(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure ClearablePromise: CLEARABLE_PROMISE =
struct

datatype 'a t = T of (unit -> 'a) * 'a Promise.t

fun clear (T (f, p)) = Promise.reset (p, f)

fun delay f = T (f, Promise.delay f)

exception Force = Promise.Force

fun force (T (_, p)) = Promise.force p

end
