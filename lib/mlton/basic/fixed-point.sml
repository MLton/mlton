structure FixedPoint: FIXED_POINT =
struct

fun fix{start, step, equals} =
   let
      fun loop s =
	 let val s' = step s
	 in if equals(s, s')
	       then s
	    else loop s'
	 end
   in loop start
   end

fun fix' f =
   let
      fun loop() =
	 let val changed = ref false
	 in f(fn () => changed := true);
	    if !changed
	       then loop()
	    else ()
	 end
   in loop()
   end


end
