(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* From Tom 7 <twm@andrew.cmu.edu>. *)
(* Implementation of the SYSLOG interface using MLton FFI.
 * This will only work in MLton.
 *)

structure MLtonSyslog :> MLTON_SYSLOG =
struct

open PrimitiveFFI.MLton.Syslog

type openflag = C_Int.t

local 
   open Logopt
in
   val CONS = LOG_CONS
   val NDELAY = LOG_NDELAY
   val NOWAIT = LOG_NOWAIT
   val ODELAY = LOG_ODELAY
(* NOT STANDARD *)
   val PERROR = LOG_PERROR
(* *)
   val PID = LOG_PID
end

type facility = C_Int.t

local
   open Facility
in
   val AUTHPRIV = LOG_AUTH
   val CRON = LOG_CRON
   val DAEMON = LOG_DAEMON
   val KERN = LOG_KERN
   val LOCAL0 = LOG_LOCAL0
   val LOCAL1 = LOG_LOCAL1
   val LOCAL2 = LOG_LOCAL2
   val LOCAL3 = LOG_LOCAL3
   val LOCAL4 = LOG_LOCAL4
   val LOCAL5 = LOG_LOCAL5
   val LOCAL6 = LOG_LOCAL6
   val LOCAL7 = LOG_LOCAL7
   val LPR = LOG_LPR
   val MAIL = LOG_MAIL
   val NEWS = LOG_NEWS
(* NOT STANDARD *)
   val SYSLOG = LOG_SYSLOG
(* *)
   val USER = LOG_USER
   val UUCP = LOG_UUCP
end

type loglevel = C_Int.t

local
   open Severity
in
   val ALERT = LOG_ALERT
   val CRIT = LOG_CRIT
   val DEBUG = LOG_DEBUG
   val EMERG = LOG_EMERG
   val ERR = LOG_ERR
   val INFO = LOG_INFO
   val NOTICE = LOG_NOTICE
   val WARNING = LOG_WARNING
end

val openlog = fn (s, opt, fac) =>
   let 
      val optf = foldl C_Int.orb 0 opt
   in
     openlog (NullString.nullTerm s, optf, fac)
   end

val closelog = fn () => 
   closelog ()

val log = fn (lev, msg) => 
   syslog (lev, NullString.nullTerm msg)

end
