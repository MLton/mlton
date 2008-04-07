(* Modified from SML/NJ sources by Stephen Weeks 1998-06-25 *)
(* modified by Stephen Weeks 1999-12-10 *)
(* modified by Stephen Weeks 2000-01-18 *)
(* modified by Matthew Fluet 2008-03-02 *)
(* modified by Matthew Fluet 2008-04-06 *)

(* os-process.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The Posix-based implementation of the generic process control
 * interface (OS.Process).
 *
 *)

structure OS_Process: OS_PROCESS_EXTRA =
   struct
      open Posix.Process

      structure Status =
         struct
            open MLtonProcess.Status

            fun equals (s1, s2) =
               (toRep s1) = (toRep s2)

            val fromPosix =
               fn es =>
               let
                  datatype z = datatype Posix.Process.exit_status
               in
                  case es of
                     W_EXITED => success
                   | W_EXITSTATUS w => 
                        fromRep (C_Status.castFromSysWord (Word8.castToSysWord w))
                   | W_SIGNALED _ => failure
                   | W_STOPPED _ => failure
               end
         end

      type status = Status.t

      val failure = Status.failure
      val success = Status.success
      fun isSuccess st = Status.equals (st, success)

      fun system cmd =
         (Status.fromRep o Posix.Error.SysCall.simpleResult)
         (fn () =>
          PrimitiveFFI.Posix.Process.system (NullString.nullTerm cmd))

      val atExit = MLtonProcess.atExit

      val exit = MLtonProcess.exit

      fun terminate x = Posix.Process.exit (Word8.fromInt (Status.toInt x))

      val getEnv = Posix.ProcEnv.getenv

      fun sleep t =
         if Time.<= (t, Time.zeroTime)
            then ()
         else sleep (Posix.Process.sleep t)
   end
