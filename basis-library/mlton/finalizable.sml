structure MLtonFinalizable: MLTON_FINALIZABLE =
struct

structure List =
   struct
      open List

      fun push (l, x) = l := x :: !l

      fun foreach (l, f) = app f l
   end

datatype 'a t = T of {afters: (unit -> unit) list ref,
		      finalizers: ('a -> unit) list ref,
		      refCount: int ref,
		      value: 'a ref}

fun touch (r: 'a ref) =
   if r = ref (!r)
      then raise Fail "Finalize.touch bug\n"
   else ()
	    
fun withValue (T {value, ...}, f) =
   DynamicWind.wind (fn () => f (!value),
		     fn () => touch value)

fun addFinalizer (T {finalizers, ...}, f) =
   List.push (finalizers, f)

(* dec is careful to keep "value" out of the closure. *)
fun dec (T {afters, finalizers, refCount, value}) =
   let
      val v = !value
   in
      fn () =>
      let
	 val n = !refCount
      in
	 if n > 0
	    then refCount := n - 1
	 else (List.foreach (!finalizers, fn f => f v)
	       ; List.foreach (!afters, fn f => f ()))
      end
   end

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

fun new v =
   let
      val afters = ref []
      val finalizers = ref []
      val refCount = ref 0
      val value = ref v
      val f = T {afters = afters,
		 finalizers = finalizers,
		 refCount = refCount,
		 value = value}
      val weak = MLtonWeak.new value
      fun isAlive () = isSome (MLtonWeak.get weak)
      val _ = finalize {clean = dec f, isAlive = isAlive}
   in
      f
   end

fun finalizeBefore (T {afters, ...}, f as T {refCount, ...}) =
   (refCount := 1 + !refCount
    ; List.push (afters, dec f))

end

