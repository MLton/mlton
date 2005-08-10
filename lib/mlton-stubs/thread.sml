(* Copyright (C) 2002-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.int

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

      type 'a t = unit

      structure Runnable =
	 struct
	    type t = unit
	 end

      val atomicSwitch = fn _ => raise Fail "Thread.atomicSwitch"
      val new = fn _ => raise Fail "Thread.new"
      val prepare = fn _ => raise Fail "Thread.prepare"
      val prepend = fn _ => raise Fail "Thread.prepend"
      val switch = fn _ => raise Fail "Thread.switch"
   end
