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

      val new = fn _ => raise Fail "Thread.new"
      val prepend = fn _ => raise Fail "Thread.prepend"
      val switch = fn _ => raise Fail "Thread.switch"
      val switch' = fn _ => raise Fail "Thread.switch'"
   end
