structure MLtonProfileTime: MLTON_PROFILE =
struct

structure Prim = Primitive.MLton.ProfileTime
structure P = MLtonProfile (open Prim)
open P

val _ =
   if not isOn
      then ()
   else
      let
	 fun setItimer (t: Time.time): unit =
	    MLtonItimer.set' (MLtonItimer.Prof, {interval = t, value = t})
	 fun init () =
	    (Prim.init ()
	     ; setCurrent (Data.malloc ())
	     ; setItimer (Time.fromMilliseconds 10))
	 val _ =
	    (* It is important to zero the itimer before the cleanAtExit,
	     * which frees the data.  Otherwise, the signal will keep arriving
	     * and the catcher (see profile-time.c) will get a segfault trying
	     * to update a nonexistent array.
	     *)
	    Cleaner.addNew
	    (Cleaner.atExit, fn () =>
	     (setItimer Time.zeroTime
	      ; P.cleanAtExit ()))
	 val _ =
	    (* It is important to cleanAtLoadWorld (which resets the profile
	     * infrastructure, before creating new profiling data and setting
	     * the timer.
	     *)
	    Cleaner.addNew
	    (Cleaner.atLoadWorld, fn () =>
	     let
		val _ = P.cleanAtLoadWorld ()
		val _ = init ()
	     in
		()
	     end)
      in
	 init ()
      end

end

