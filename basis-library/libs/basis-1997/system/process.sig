signature OS_PROCESS_1997 =
   sig
      (* VIOLATION *)
(*
      eqtype status
*)
      type status

      val atExit: (unit -> unit) -> unit 
      val exit: status -> 'a 
      val failure: status 
      val getEnv: string -> string option
      val success: status 
      val system: string -> status 
      val terminate: status -> 'a 
   end
