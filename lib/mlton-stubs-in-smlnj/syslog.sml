(* From Tom 7 <twm@andrew.cmu.edu>. *)
(* Implementation of Syslog which doesn't log anything.
 * Use for compiling (type-checking) with SML/NJ for its
 * superior error messages.
 *)

structure Syslog :> MLTON_SYSLOG =
struct

    type openflag = unit
        
    val CONS = ()
    val NDELAY = ()
    val PERROR = ()
    val PID = ()

    type facility = unit

    val AUTHPRIV = ()
    val CRON = ()
    val DAEMON = ()
    val KERN = ()
    val LOCAL0 = ()
    val LOCAL1 = ()
    val LOCAL2 = ()
    val LOCAL3 = ()
    val LOCAL4 = ()
    val LOCAL5 = ()
    val LOCAL6 = ()
    val LOCAL7 = ()
    val LPR = ()
    val MAIL = ()
    val NEWS = ()
    val SYSLOG = ()
    val USER = ()
    val UUCP = ()

    type loglevel = unit

    val EMERG = ()
    val ALERT = ()
    val CRIT = ()
    val ERR = ()
    val WARNING = ()
    val NOTICE = ()
    val INFO = ()
    val DEBUG = ()

    fun openlog _ = ()

    fun closelog _ = ()

    fun log _ = ()
end
