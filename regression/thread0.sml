open MLton.Thread

val _ =
   if 13 = 1 + switch(fn t => prepVal (t, 12))
      then print "1 succeeded\n"
   else ()

val _ =
   if 13 = 1 + switch(fn t =>
		      prep (new(fn () => switch(fn _ => prepVal (t, 12)))))
      then print "2 succeeded\n"
   else ()
      
val _ =
   if 13 = switch(fn t => prepVal (prepend(t, fn n => n + 1), 12))
      then print "3 succeeded\n"
   else ()

val _ =
   if 13 = switch(fn t =>
		  prep (new(fn () =>
			    let val t = prepend(t, fn n => n + 1)
			    in switch(fn _ => prepVal (t, 12))
			    end)))
      then print "4 succeeded\n"
   else ()
