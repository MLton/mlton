 fun downto n =
    if n = 0
       then 0
    else downto (n - 1)

val truee = 1 = downto 0

open Posix.ProcEnv

val egid = getegid ()
val env = getenv "HOME"
val euid = geteuid ()
val gid = getgid ()
val groups = getgroups ()
val login = getlogin () handle _ => "<login>"
val pgrp = getpgrp ()
val pid = getpid ()
val ppid = getppid ()
val uid = getuid ()

val () = setgid gid
val () = setpgid {pgid = SOME pid, pid = SOME pid}
val () = ignore (setsid ()) handle _ => ()
val () = setuid uid
