structure Date =
   struct
      open Date OpenInt32

      val date =
	 fn {year, month, day, hour, minute, second, offset} =>
	 date{year = toInt year,
	      month = month,
	      day = toInt day,
	      hour = toInt hour,
	      minute = toInt minute,
	      second = toInt second,
	      offset = offset}
      val year = fromInt o year
      val day = fromInt o day
      val hour = fromInt o hour
      val minute = fromInt o minute
      val second = fromInt o second
      val yearDay = fromInt o yearDay
      val fromString: string -> date option = fn _ => raise Fail "fromString"
      val localOffset: unit -> Time.time = fn _ => raise Fail "localOffset"
      val scan: (char, 'a) StringCvt.reader -> 'a -> (date * 'a) option =
	 fn _ => raise Fail "scan"
   end
