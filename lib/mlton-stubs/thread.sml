structure Thread =
   struct
      type 'a t = unit

      val atomicBegin = fn _ => raise Fail "Thread.atomicBegin"
      val atomicEnd = fn _ => raise Fail "Thread.atomicEnd"
      val new = fn _ => raise Fail "Thread.new"
      val prepend = fn _ => raise Fail "Thread.prepend"
      val switch = fn _ => raise Fail "Thread.switch"
      val switch' = fn _ => raise Fail "Thread.switch'"
   end
