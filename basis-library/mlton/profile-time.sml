structure ProfileTime: MLTON_PROFILE =
struct

structure Prim = Primitive.MLton.ProfileTime
structure P = Profile (Prim)
open P
   
fun setItimer (t: Time.time): unit =
   Itimer.set' (Itimer.Prof, {interval = t, value = t})

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

