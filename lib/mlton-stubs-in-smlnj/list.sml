structure List =
   struct
      open List OpenInt32

      val length = fn z => fromInt(length z)
      local
	 fun make f (l, n) = f (l, toInt n)
      in
	 val nth = fn z => make List.nth z
	 val take = fn z => make List.take z
	 val drop = fn z => make List.drop z
      end
      fun tabulate(n, f) = List.tabulate(toInt n, f o fromInt)
   end
