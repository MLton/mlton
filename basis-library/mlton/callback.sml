structure MLtonCallback :> MLTON_CALLBACK_EXTRA =
  struct
    structure Prim = Primitive.MLton.Callback

    structure Type =
      struct
	type rep = char list	
	val zeroRep = []

	type 'a inc = rep -> rep
	type 'a fetch = rep -> 'a
	type 'a ret = 'a -> unit
	type 'a IF = ('a inc) * ('a fetch) 
	type 'a IFR = ('a inc) * ('a fetch) * ('a ret)
	fun ifrInc (inc,fetch,ret) = inc
	fun ifrFetch (inc,fetch,ret) = fetch
	fun ifrRet (inc,fetch,ret) = ret
	local
	  fun mkFetchIncRet
	      (name: char,
	       fetch: int -> 'a,
	       ret: 'a -> unit) : 'a IFR =
	    (fn rep => name::rep,
	     fn rep => 
	     fetch (List.foldl
		    (fn (c, index) => if c = name
					then index + 1
					else index)
		    0 rep),
	     ret)
	in
	  val ifrB : bool IFR = mkFetchIncRet (#"B", Prim.fetchB, Prim.retB)
	  val ifrC : char IFR = mkFetchIncRet (#"C", Prim.fetchC, Prim.retC)
	  val ifrI : int IFR = mkFetchIncRet (#"I", Prim.fetchI, Prim.retI)
	  val ifrR : real IFR = mkFetchIncRet (#"R", Prim.fetchR, Prim.retR)
	  val ifrU : unit IFR = mkFetchIncRet (#"U", fn i => (), fn () => ())
	  val ifrW : word IFR = mkFetchIncRet (#"W", Prim.fetchW, Prim.retW)
	end

	type 'b paused = unit -> (unit -> 'b)
	type ('a, 'b) ppaused = 'a -> 'b paused
	type ('a, 'b) arg = rep -> rep * ('a -> 'b, 'b) ppaused
	type 'b res = rep -> rep * (unit -> 'b, unit) ppaused

	local
	  fun make (inc: 'a inc, fetch: 'a fetch) : ('a, 'b) arg =
	    fn (rep: rep) =>
	    (inc rep,
	     fn f => fn () =>
	     let val b = fetch rep
	     in fn () => f b end)
	  fun mk (ifr: 'a IFR) =
	    make (ifrInc ifr, ifrFetch ifr)
	in
	  val B : (bool, 'b) arg = fn rep => mk ifrB rep
	  val C : (char, 'b) arg = fn rep => mk ifrC rep
	  val I : (int, 'b) arg = fn rep => mk ifrI rep
	  val R : (real, 'b) arg = fn rep => mk ifrR rep
 	  val U : (unit, 'b) arg = fn rep => mk ifrU rep
	  val W : (word, 'b) arg = fn rep => mk ifrW rep
	end

	infixr -->
	fun (X: ('a, 'b) arg) --> (Y: 'b res) : ('a -> 'b) res =
	  fn (rep: rep) =>
	  let 
	    val (rep: rep, X: ('a -> 'b, 'b) ppaused) = X rep
	    val (rep: rep, Y: (unit -> 'b, unit) ppaused) = Y rep
	  in
	    (rep,
	     fn (F : unit -> ('a -> 'b)) =>
	     let 
	       val f: 'b paused = X (F ())
	     in 
	       fn () => 
	       Y (f ()) ()
	     end)
	  end
	
	local
	  fun make' (inc: 'a inc, ret: 'a ret) : 'a res = 
	    fn (rep: rep) =>
	    (inc rep,
	     fn f => fn () => fn () =>
	     let val v = f () in
	       MLtonThread.atomicBegin ()
	       ; ret v
	     end)
	  fun mk' (ifr: 'a IFR) =
	    make' (ifrInc ifr, ifrRet ifr)
	in
	  val B' : bool res = mk' ifrB
	  val C' : char res = mk' ifrC
	  val I' : int res = mk' ifrI
	  val R' : real res = mk' ifrR
	  val U' : unit res = mk' ifrU
	  val W' : word res = mk' ifrW
	end
	  
	fun make (ty: ('a -> 'b) res) : ('a -> 'b) -> ((unit -> unit) * string) = 
	  fn (f: 'a -> 'b) =>
	  let 
	    val (rep: rep, ppaused: (unit -> 'a -> 'b, unit) ppaused) = 
	      ty zeroRep
	    val f = ppaused (fn () => f) 
	  in
	    (fn () =>
	     let val f = f () in
	       MLtonThread.atomicEnd ()
	       ; f ()
	     end,
	     implode (rev rep))
	  end
      end

    val registered : (string * ((unit -> unit) * string)) list ref = ref []

    fun pred (n:string) = (fn (n',_) => n = n')
    fun isRegistered n =
      List.exists (pred n) (!registered)
    fun unregister n =
      registered := List.filter (not o (pred n)) (!registered)
    val register' =
      let
	val _ = 
	  MLtonThread.setCallFromCHandler
	  (fn () =>
	   let 
	     val cs = Prim.callbackName ()
	     val n = if Primitive.Cpointer.isNull cs
		       then raise Fail ("null callback function")
		       else C.CS.toString cs
	     val cs = Prim.callbackType ()
	     val ty = if Primitive.Cpointer.isNull  cs
			then raise Fail ("null callback type")
			else C.CS.toString cs
	   in
	     case List.find (pred n) (!registered) of
	       SOME (_,(f,rep)) => 
		 if rep = ty
		   then f ()
		   else raise Fail ("callback function type mismatch: " ^ n)
	     | NONE => raise Fail ("unregistered callback function: " ^ n)
	   end)
      in
	fn (n,frep) => 
	(unregister n
	 ; registered := (n,frep)::(!registered))
      end
    fun register (n,ty) f =
      register' (n, Type.make ty f)
  end
