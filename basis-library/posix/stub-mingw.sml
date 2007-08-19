(* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Stub out functions that are not implemented on MinGW. *)
local
   structure Error = PosixError
   val stub: string * ('a -> 'b) -> ('a -> 'b) =
      fn (msg, f) => 
      if let open Primitive.MLton.Platform.OS in MinGW = host end
         then fn _ => (if true 
                          then ()
                          else (PrimitiveFFI.Stdio.print msg
                                ; PrimitiveFFI.Stdio.print "\n")
                       ; Error.raiseSysWithMsg (Error.nosys, msg))
         else f
in
   structure PrimitiveFFI =
      struct
         open PrimitiveFFI

         structure MLton =
            struct
               open MLton

               structure Itimer =
                  struct
                     open Itimer

                     val set = stub ("set", set)
                  end
            end

         structure OS =
            struct
               open OS

               structure IO =
                  struct
                     open IO

                     val poll = stub ("poll", poll)
                  end
            end

         structure Posix =
            struct
               open Posix

               structure FileSys =
                  struct
                     open FileSys

                     val chown = stub ("chown", chown)
                     val fchown = stub ("fchown", fchown)
                     val fpathconf = stub ("fpathconf", fpathconf)
                     val link = stub ("link", link)
                     val mkfifo = stub ("mkfifo", mkfifo)
                     val pathconf = stub ("pathconf", pathconf)
                     val readlink = stub ("readlink", readlink)
                     val symlink = stub ("symlink", symlink)
                  end

               structure IO =
                  struct
                     open IO

                     val fcntl2 = stub ("fcntl2", fcntl2)
                     val fcntl3 = stub ("fcntl3", fcntl3)
                  end

               structure ProcEnv =
                  struct
                     open ProcEnv

                     val ctermid = stub ("ctermid", ctermid)
                     val getegid = stub ("getegid", getegid)
                     val geteuid = stub ("geteuid", geteuid)
                     val getgid = stub ("getgid", getgid)
                     val getgroups = stub ("getgroups", getgroups)
                     val getlogin = stub ("getlogin", getlogin)
                     val getpgrp = stub ("getpgrp", getpgrp)
                     val getppid = stub ("getppid", getppid)
                     val getuid = stub ("getuid", getuid)
                     val setgid = stub ("setgid", setgid)
                     val setgroups = stub ("stegroups", setgroups)
                     val setpgid = stub ("setpgid", setpgid)
                     val setsid = stub ("setsid", setsid)
                     val setuid = stub ("setuid", setuid)
                     val sysconf = stub ("sysconf", sysconf)
                     val times = stub ("times", times)
                     val ttyname = stub ("ttyname", ttyname)
                  end

               structure Process =
                  struct
                     open Process

                     val exece = stub ("exece", exece)
                     val execp = stub ("execp", execp)
                     val exit = stub ("exit", exit)
                     val fork = stub ("fork", fork)
                     val kill = stub ("kill", kill)
                     val pause = stub ("pause", pause)
                     val waitpid = stub ("waitpid", waitpid)
                  end

               structure SysDB =
                  struct
                     open SysDB

                     val getgrgid = stub ("getgrgid", getgrgid)
                     val getgrnam = stub ("getgrnam", getgrnam)
                     val getpwuid = stub ("getpwuid", getpwuid)
                  end

               structure TTY =
                  struct
                     open TTY

                     structure TC =
                        struct
                           open TC

                           val drain = stub ("drain", drain)
                           val flow = stub ("flow", flow)
                           val flush = stub ("flush", flush)
                           val getattr = stub ("getattr", getattr)
                           val getpgrp = stub ("getpgrp", getpgrp)
                           val sendbreak = stub ("sendbreak", sendbreak)
                           val setattr = stub ("setattr", setattr)
                           val setpgrp = stub ("setpgrp", setpgrp)
                        end
                  end
            end

         structure Socket =
            struct
               open Socket

               structure UnixSock =
                  struct
                     open UnixSock

                     val toAddr = stub ("toAddr", toAddr)
                     val fromAddr = stub ("fromAddr", fromAddr)
                  end
            end
      end
end
