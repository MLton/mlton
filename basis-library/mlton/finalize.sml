structure MLtonFinalize: MLTON_FINALIZE =
struct

val finalize =
   let
      val r: {clean: unit -> unit,
	      isAlive: unit -> bool} list ref = ref []
      fun clean l =
	 List.foldl (fn (z as {clean, isAlive}, (gotOne, zs)) =>
		     if isAlive ()
			then (gotOne, z :: zs)
		     else (clean (); (true, zs)))
	 (false, []) l
      val exiting = ref false
      val _ = MLtonSignal.handleGC (fn () => r := #2 (clean (!r)))
      val _ =
	 Cleaner.addNew
	 (Cleaner.atExit, fn () =>
	  let
	     val l = !r
	     (* Must clear r so that the handler doesn't interfere and so that
	      * all other references to the finalizers are dropped.
	      *)
	     val _ = r := []
	     fun loop l =
		let
		   val _ = MLtonGC.collect ()
		   val (gotOne, l) = clean l
		in
		   if gotOne
		      then loop l
		   else ()
		end
	  in
	     loop l
	  end)
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
