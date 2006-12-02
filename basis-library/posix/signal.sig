signature POSIX_SIGNAL =
   sig
      eqtype signal

      val toWord: signal -> SysWord.word
      val fromWord: SysWord.word -> signal

      val abrt: signal
      val alrm: signal
      val bus: signal
      val fpe: signal
      val hup: signal
      val ill: signal
      val int: signal
      val kill: signal
      val pipe: signal
      val quit: signal
      val segv: signal
      val term: signal
      val usr1: signal
      val usr2: signal
      val chld: signal
      val cont: signal
      val stop: signal
      val tstp: signal
      val ttin: signal
      val ttou: signal
   end

signature POSIX_SIGNAL_EXTRA =
   sig
      include POSIX_SIGNAL

      val prof: signal
      val vtalrm: signal

      val fromInt: int -> signal
      val toInt: signal -> int
   end
