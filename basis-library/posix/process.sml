(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PosixProcess: POSIX_PROCESS_EXTRA =
   struct
      structure Prim = PrimitiveFFI.Posix.Process
      open Prim
      structure Error = PosixError
      structure SysCall = Error.SysCall

      type signal = PosixSignal.signal
      type pid = C_PId.t

      val wordToPid = C_PId.castFromSysWord
      val pidToWord = C_PId.castToSysWord

      fun fork () =
         SysCall.syscall'
         ({errVal = C_PId.castFromFixedInt ~1}, fn () =>
          (Prim.fork (), fn p =>
           if p = C_PId.castFromFixedInt 0 then NONE else SOME p))

      val fork =
         if Primitive.MLton.Platform.OS.forkIsEnabled
            then fork
         else fn () => Error.raiseSys Error.nosys

      val conv = NullString.nullTerm
      val convs = CUtil.StringVector.fromList

      fun exece (path, args, env): 'a =
         let
            val path = conv path
            val args = convs args
            val env = convs env
         in
            (SysCall.simple
             (fn () => Prim.exece (path, 
                                   #1 args, #2 args, #3 args, 
                                   #1 env, #2 env, #3 env))
             ; raise Fail "Posix.Process.exece")
         end

      fun exec (path, args): 'a =
         exece (path, args, PosixProcEnv.environ ())

      fun execp (file, args): 'a =
         let
            val file = conv file
            val args = convs args
         in
            (SysCall.simple 
             (fn () => Prim.execp (file, 
                                   #1 args, #2 args, #3 args))
             ; raise Fail "Posix.Process.execp")
         end

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

      fun fromStatus status =
         if Prim.ifExited status <> C_Int.zero
            then (case Prim.exitStatus status of
                     0 => W_EXITED
                   | n => W_EXITSTATUS (Word8.castFromSysWord (C_Int.castToSysWord n)))
         else if Prim.ifSignaled status <> C_Int.zero
            then W_SIGNALED (Prim.termSig status)
         else if Prim.ifStopped status <> C_Int.zero
            then W_STOPPED (Prim.stopSig status)
         else raise Fail "Posix.Process.fromStatus"

      structure W =
         struct
            structure Flags = BitFlags(structure S = C_Int)
            open W Flags
            (* val continued = CONTINUED *)
            val nohang = NOHANG
            val untraced = UNTRACED
         end

      local
         val status: C_Status.t ref = ref (C_Status.fromInt 0)
         fun wait (wa, status, flags) =
            let
               val useCwait = 
                  Primitive.MLton.Platform.OS.useWindowsProcess
                  andalso case wa of W_CHILD _ => true | _ => false
               val pid =
                  case wa of
                     W_ANY_CHILD => C_PId.castFromFixedInt ~1
                   | W_CHILD pid => pid
                   | W_SAME_GROUP => C_PId.castFromFixedInt 0
                   | W_GROUP pid => C_PId.~ pid
               val flags = W.flags flags
            in
               SysCall.simpleResultRestart'
               ({errVal = C_PId.castFromFixedInt ~1}, fn () =>
                let
                   val pid = 
                      if useCwait 
                         then PrimitiveFFI.MLton.Process.cwait (pid, status)
                      else Prim.waitpid (pid, status, flags)
                in
                   pid
                end)
            end
         fun getStatus () = fromStatus (!status)
      in
         fun waitpid (wa, flags) =
            let
               val pid = wait (wa, status, flags)
            in 
               (pid, getStatus ())
            end

         fun waitpid_nh (wa, flags) =
            let
               val pid = wait (wa, status, W.nohang :: flags)
            in
               if C_PId.castFromFixedInt 0 = pid
                  then NONE
               else SOME (pid, getStatus ())
            end
      end

      fun wait () = waitpid (W_ANY_CHILD, [])

      fun exit (w: Word8.word): 'a  =
         (* Posix.Process.exit does not call atExit cleaners, as per the basis
          * library spec.
          *)
         (Prim.exit (C_Status.castFromSysWord (Word8.castToSysWord w))
          ; raise Fail "Posix.Process.exit")

      datatype killpid_arg  =
         K_PROC of pid
       | K_SAME_GROUP
       | K_GROUP of pid 

      fun kill (ka: killpid_arg, s: signal): unit =
         let
            val pid =
               case ka of
                  K_PROC pid => pid
                | K_SAME_GROUP => C_PId.castFromFixedInt ~1
                | K_GROUP pid => C_PId.~ pid
         in
            SysCall.simple (fn () => Prim.kill (pid, s))
         end

      local
         fun wrap prim (t: Time.time): Time.time =
            Time.fromSeconds
            (C_UInt.toLargeInt
             (prim 
              ((C_UInt.fromLargeInt (Time.toSeconds t))
               handle Overflow => Error.raiseSys Error.inval)))
      in
         val alarm = wrap Prim.alarm
         (* val sleep = wrap Prim.sleep *)
      end

      fun sleep (t: Time.time): Time.time =
         let
            val t = Time.toNanoseconds t
            val sec = LargeInt.quot (t, 1000000000)
            val nsec = LargeInt.rem (t, 1000000000)
            val (sec, nsec) =
               (C_Time.fromLargeInt sec, C_Long.fromLargeInt nsec)
               handle Overflow => Error.raiseSys Error.inval
            val secRem = ref sec
            val nsecRem = ref nsec
            fun remaining _ =
               Time.+ (Time.fromSeconds (C_Time.toLargeInt (!secRem)),
                       Time.fromNanoseconds (C_Long.toLargeInt (!nsecRem)))
         in
            SysCall.syscallErr
            ({clear = false, restart = false, errVal = C_Int.fromInt ~1}, fn () =>
             {handlers = [(Error.intr, remaining)],
              post = remaining,
              return = Prim.nanosleep (secRem, nsecRem)})
         end

      (* FIXME: pause *)
      fun pause () =
         SysCall.syscallErr
         ({clear = false, restart = false, errVal = C_Int.fromInt ~1},
          fn () =>
          {return = Prim.pause (),
           post = fn _ => (),
           handlers = [(Error.intr, fn () => ())]})
   end
