structure ProfileTime: MLTON_PROFILE =
struct

fun setItimer (t: Time.time): unit =
   Itimer.set' (Itimer.Prof, {interval = t, value = t})

(* It is important that clean () happend before the data is freed, because
 * otherwise the signal will keep arriving and the catcher (see profile-time.c)
 * will get a segfault trying to update a nonexistent array.
 *)
fun clean () = setItimer Time.zeroTime

structure Prim = Primitive.MLton.ProfileTime
structure P = Profile (open Prim
		       val clean = clean)
open P

val _ =
   if not isOn
      then ()
   else
      let
	 val _ = Prim.init ()
	 val _ = setCurrent (Data.malloc ())
	 val _ = setItimer (Time.fromMilliseconds 10)
      in
	 ()
      end

end

