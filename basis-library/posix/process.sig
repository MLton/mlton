signature POSIX_PROCESS =
   sig
      eqtype signal
      eqtype pid

      structure W:
         sig
            include BIT_FLAGS
            val untraced: flags 
         end

      datatype exit_status =
         W_EXITED
       | W_EXITSTATUS of Word8.word
       | W_SIGNALED of signal
       | W_STOPPED of signal 

      datatype killpid_arg =
         K_PROC of pid
       | K_SAME_GROUP
       | K_GROUP of pid 

      datatype waitpid_arg =
         W_ANY_CHILD
       | W_CHILD of pid
       | W_SAME_GROUP
       | W_GROUP of pid 

      val alarm: Time.time -> Time.time 
      val exec: string * string list -> 'a 
      val exece: string * string list * string list -> 'a 
      val execp: string * string list -> 'a 
      val exit: Word8.word -> 'a 
      val fork: unit -> pid option 
      val fromStatus: OS.Process.status -> exit_status
      val kill: killpid_arg * signal -> unit 
      val pause: unit -> unit 
      val pidToWord: pid -> SysWord.word 
      val sleep: Time.time -> Time.time 
      val wait: unit -> pid * exit_status
      val waitpid: waitpid_arg * W.flags list -> pid * exit_status
      val waitpid_nh: waitpid_arg * W.flags list -> (pid * exit_status) option 
      val wordToPid: SysWord.word -> pid 
   end

signature POSIX_PROCESS_EXTRA = 
   sig
      include POSIX_PROCESS
   end
