structure Main = Main ()

val _ =
   let
      open Trace.Immediate
   in
      debug := Out Out.error
      ; flagged ()
      ; on []
   end
