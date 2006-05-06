signature OS_PROCESS =
   sig
      type status

      val atExit: (unit -> unit) -> unit
      val exit: status -> 'a
      val failure: status
      val getEnv: string -> string option
      val isSuccess: status -> bool
      val sleep: Time.time -> unit
      val success: status
      val system: string -> status
      val terminate: status -> 'a
   end

signature OS_PROCESS_EXTRA =
   sig
      include OS_PROCESS

      structure Status:
         sig
            type t = status

            val fromInt: int -> t
            val fromPosix: Posix.Process.exit_status -> t
         end
   end
