(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

(* From Tom 7 <twm@andrew.cmu.edu>. *)
(* A rather complete interface to the syslog facilities. 
 *
 * See
 * man 3 syslog
 *
 *  .. for descriptions of these constants.
 *)
signature MLTON_SYSLOG =
   sig
      type openflag
	 
      val CONS     : openflag
      val NDELAY   : openflag
      val PERROR   : openflag
      val PID      : openflag

      type facility

      val AUTHPRIV : facility
      val CRON     : facility
      val DAEMON   : facility
      val KERN     : facility
      val LOCAL0   : facility
      val LOCAL1   : facility
      val LOCAL2   : facility
      val LOCAL3   : facility
      val LOCAL4   : facility
      val LOCAL5   : facility
      val LOCAL6   : facility
      val LOCAL7   : facility
      val LPR      : facility
      val MAIL     : facility
      val NEWS     : facility
      val SYSLOG   : facility
      val USER     : facility
      val UUCP     : facility

      type loglevel

      val EMERG    : loglevel
      val ALERT    : loglevel
      val CRIT     : loglevel
      val ERR      : loglevel
      val WARNING  : loglevel
      val NOTICE   : loglevel
      val INFO     : loglevel
      val DEBUG    : loglevel

      (* Closelog is also optional. *)
      val closelog: unit -> unit

      (* log a message at a particular loglevel. *)
      val log: loglevel * string -> unit

      (*
       * Openlog opens a connection to the system logger.
       * Calling openlog is optional but recommended.
       * From the man pages.
       * The string is prefixed to each message, and is typically set to the
       * program name.
       *)
      val openlog: string * openflag list * facility -> unit
   end
