structure MLtonPtrace: MLTON_PTRACE =
   struct
      open Primitive.Ptrace
	 
      type pid = Pid.t
	 
      local
	 fun make request pid = PosixError.checkResult(ptrace2(request, pid))
      in
	 val attach = make ATTACH
	 val cont = make CONT
	 val detach = make DETACH
	 val kill = make KILL
	 val singleStep = make SINGLESTEP
	 val sysCall = make SYSCALL
      end

      local
      in
	 fun peekText(pid, addr) =
	    let val data: word ref = ref 0w0
	    in PosixError.checkResult(ptrace4(PEEKTEXT, pid, addr, data))
	       ; !data
	    end
      end
   end
