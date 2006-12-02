(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure SMLofNJ: SML_OF_NJ =
   struct
      structure Cont =
         struct
            structure C = MLton.Cont

            type 'a cont = 'a C.t
            val callcc = C.callcc
            fun throw k v = C.throw (k, v)
         end

      structure SysInfo =
         struct
            exception UNKNOWN
            datatype os_kind = BEOS | MACOS | OS2 | UNIX | WIN32

            fun getHostArch () =
               MLton.Platform.Arch.toString MLton.Platform.Arch.host

            fun getOSKind () =
               let
                  open MLton.Platform.OS
               in
                  case host of
                     AIX => UNIX
                   | Cygwin => UNIX
                   | Darwin => MACOS
                   | FreeBSD => UNIX
                   | HPUX => UNIX
                   | Linux => UNIX
                   | MinGW => WIN32
                   | NetBSD => UNIX
                   | OpenBSD => UNIX
                   | Solaris => UNIX
               end

            fun getOSName () = MLton.Platform.OS.toString MLton.Platform.OS.host
         end

      val getCmdName = CommandLine.name
      val getArgs = CommandLine.arguments

      fun getAllArgs () = getCmdName () :: getArgs ()

      val exnHistory = MLton.Exn.history

      fun exportFn (file: string, f) =
         let
            open MLton.World OS.Process
         in
            case save (file ^ ".mlton") of
               Original => exit success
             | Clone => exit (f (getCmdName (), getArgs ()) handle _ => failure)
         end

      fun exportML (f: string): bool =
         let
            open MLton.World
         in
            case save (f ^ ".mlton") of
               Clone => true
             | Original => false
         end
   end
