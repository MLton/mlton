fun doit tz =
   let
      val () = MLton.ProcEnv.setenv {name = "TZ", value = tz}
      val lo = Date.localOffset ()
   in
      app print
      ["TZ = ", tz, " :: Date.localOffset () = ", Time.toString lo, "\n"]
   end

val () = doit "UTC+4"
val () = doit "UTC+0"
val () = doit "UTC-4"
