structure MLtonFinalize: MLTON_FINALIZE =
struct

val finalize =
   let
      val r: {clean: unit -> unit,
	      isAlive: unit -> bool} list ref = ref []
      val _ =
	 MLtonSignal.handleGC
	 (fn () =>
	  r := (List.foldl (fn (z as {clean, isAlive}, ac) =>
			    if isAlive ()
			       then z :: ac
			    else (clean (); ac))
		[] (!r)))
   in
      fn z => r := z :: !r
   end

val finalize =
   fn (a: 'a, f: unit -> unit) =>
   let
      val w = MLtonWeak.new a
      fun isAlive () = isSome (MLtonWeak.get w)
   in
      finalize {clean = f, isAlive = isAlive}
   end

end
