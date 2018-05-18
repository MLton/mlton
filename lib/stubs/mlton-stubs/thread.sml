(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonThread =
   struct
      structure AtomicState =
         struct
            datatype t = NonAtomic | Atomic of int
         end
      val atomicBegin = fn _ => raise Fail "Thread.atomicBegin"
      val atomicEnd = fn _ => raise Fail "Thread.atomicEnd"
      val atomically = fn _ => raise Fail "Thread.atomically"
      val atomicState = fn _ => raise Fail "Thread.atomicState"

      type 'a t = exn

      structure Runnable =
         struct
            type t = exn
         end

      val atomicSwitch = fn _ => raise Fail "Thread.atomicSwitch"
      val new = fn _ => raise Fail "Thread.new"
      val prepare = fn _ => raise Fail "Thread.prepare"
      val prepend = fn _ => raise Fail "Thread.prepend"
      val switch = fn _ => raise Fail "Thread.switch"
   end
