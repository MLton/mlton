signature OS_PROCESS =
   sig
      type status
      val success: status
      val failure: status
      val isSuccess: status -> bool
      val system: string -> status
      val atExit: (unit -> unit) -> unit
      val exit: status -> 'a
      val terminate: status -> 'a
      val getEnv: string -> string option
      val sleep: Time.time -> unit
   end

signature OS_PROCESS_EXTRA =
   sig
      include OS_PROCESS

      val wait: Posix.Process.pid -> status
   end
