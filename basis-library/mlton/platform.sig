signature MLTON_PLATFORM =
   sig
      datatype arch = Sparc | X86
      val arch: arch
	 
      datatype os = Cygwin | FreeBSD | Linux | NetBSD | SunOS
      val os: os
   end
