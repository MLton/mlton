structure Thread: MLTON_THREAD =
   struct
      exception Bug

      fun bug () = raise Bug

      open SMLofNJ.Cont
	 
      type 'a t = ((unit -> 'a) -> unit)
	 
      val base: (unit -> unit) cont =
	 callcc (fn k =>
		 let val f = callcc (fn k' => throw k k')
		 in (f () handle _ => bug ())
		    ; bug ()
		 end)

      fun new f x = callcc (fn _ => throw base (fn () => f (x ())))

      fun switch' (f: 'a t -> 'b t * (unit -> 'b)): 'a =
	 callcc (fn k: (unit -> 'a) cont =>
		 let val (t, x) = f (fn v: unit -> 'a => throw k v)
		 in t x; bug ()
		 end)
	 ()

      fun switch f =
	 switch' (fn t => let val (t, x) = f t
			  in (t, fn () => x)
			  end)

      fun prepend _ = raise Fail "prepend"
   end
