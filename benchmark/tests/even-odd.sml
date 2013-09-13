local
   fun even' 0 = true
     | even' i = odd' (i-1)
   and odd'  0 = false
     | odd'  i = even' (i-1)
in
    fun even i = even' (abs i)
    fun odd i  = odd' (abs i)
end

structure Main =
   struct
      fun doit n =
         if n = 0
            then ()
         else let
                 val _ = if (even 500000000) <> not (odd 500000000)
                            then raise Fail "bug"
                         else ()
              in
                 doit (n - 1)
              end
   end
