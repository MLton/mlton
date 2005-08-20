signature POSIX_PROCESS_1997 =
   sig
      eqtype signal
      eqtype pid

      val wordToPid: SysWord.word -> pid 
      val pidToWord: pid -> SysWord.word 
      val fork: unit -> pid option 
      val exec: string * string list -> 'a 
      val exece: string * string list * string list -> 'a 
      val execp: string * string list -> 'a 

      datatype waitpid_arg =
         W_ANY_CHILD
       | W_CHILD of pid
       | W_SAME_GROUP
       | W_GROUP of pid 

      datatype exit_status =
         W_EXITED
       | W_EXITSTATUS of Word8.word
       | W_SIGNALED of signal
       | W_STOPPED of signal 

      structure W :
         sig
            include POSIX_FLAGS_1997
            val untraced: flags 
         end

      val wait: unit -> pid * exit_status
      val waitpid: waitpid_arg * W.flags list -> pid * exit_status
      val waitpid_nh: waitpid_arg * W.flags list -> (pid * exit_status) option 
      val exit: Word8.word -> 'a 

      datatype killpid_arg  =
         K_PROC of pid
       | K_SAME_GROUP
       | K_GROUP of pid 

      val kill: killpid_arg * signal -> unit 
      val alarm: Time.time -> Time.time 
      val pause: unit -> unit 
      val sleep: Time.time -> Time.time 
   end
