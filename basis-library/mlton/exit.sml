structure Exit =
   struct
      structure Status = PosixPrimitive.Process.Status

      val exiting = ref false

      fun atExit f =
	 if !exiting
	    then ()
	 else Cleaner.addNew (Cleaner.atExit, f)

      fun exit (status: Status.t): 'a =
	 if !exiting
	    then raise Fail "exit"
	 else
	    let
	       val _ = exiting := true
	       val i = Status.toInt status
	    in
	       if 0 <= i andalso i < 256
		  then (let open Cleaner in clean atExit end
			; Primitive.halt status
			; raise Fail "exit")
	       else raise Fail (concat ["exit must have 0 <= status < 256: saw ",
					Int.toString i])
	    end
   end
