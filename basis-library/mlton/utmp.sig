signature UTMP =
   sig
      type t = {
		ty: ty,
		pid: Posix.Process.pid,
		deviceName: string,
		id: string,
		user: string,
		host: string,
		termination: int,
		exit: int,
		}
   end


