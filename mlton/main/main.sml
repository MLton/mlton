structure Main = Main ()

val _ =
   let
      open Trace.Immediate
   in
      debug := Out Out.error
      ; flagged ()
(*      ; on ["elaborateTopdec"] *)
(*      ; on ["cut", "realize", "TypeStr.toEnv"] *)
(*      ; on ["elaborateSigexp"] *)
(*       ; on ["elaborateSigexp", "elaborateSpec"] *)
(*       ; on ["elaborateType"] *)
(*       ; on ["handleStr", "handleType", "handleVal"] *)
(*       ; on ["TypeStr.toEnv", "TypeStr.fromEnv"] *)
   end
