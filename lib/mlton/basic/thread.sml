structure Thread: THREAD =
struct

open MLton.Thread

fun generate (f: ('a -> unit) -> unit): unit -> 'a option =
   let
      val paused: 'a option t option ref = ref NONE
      val gen: unit t option ref = ref NONE
      fun return (a: 'a option): unit =
	 switch (fn t' =>
		 let val _ = gen := SOME t'
		    val t = valOf (!paused)
		    val _ = paused := NONE
		 in (t, a)
		 end)
      val _ =
	 gen := SOME (new (fn () => (f (return o SOME)
				     ; return NONE)))
   in fn () => switch (fn t => (paused := SOME t
				; (valOf (!gen), ())))
   end

end
