(* From Tom 7 <twm@andrew.cmu.edu>. *)
(* Implementation of the SYSLOG interface using MLton FFI.
 * This will only work in MLton.
 *)

structure Syslog :> MLTON_SYSLOG =
struct

type openflag = int
        
val CONS = _prim "LOG_CONS" : openflag;
val NDELAY = _prim "LOG_NDELAY" : openflag;
val PERROR = _prim "LOG_PERROR" : openflag;
val PID = _prim "LOG_PID" : openflag;

type facility = int

val AUTHPRIV = _prim "LOG_AUTHPRIV" : facility;
val CRON = _prim "LOG_CRON" : facility;
val DAEMON = _prim "LOG_DAEMON" : facility;
val KERN = _prim "LOG_KERN" : facility;
val LOCAL0 = _prim "LOG_LOCAL0" : facility;
val LOCAL1 = _prim "LOG_LOCAL1" : facility;
val LOCAL2 = _prim "LOG_LOCAL2" : facility;
val LOCAL3 = _prim "LOG_LOCAL3" : facility;
val LOCAL4 = _prim "LOG_LOCAL4" : facility;
val LOCAL5 = _prim "LOG_LOCAL5" : facility;
val LOCAL6 = _prim "LOG_LOCAL6" : facility;
val LOCAL7 = _prim "LOG_LOCAL7" : facility;
val LPR = _prim "LOG_LPR" : facility;
val MAIL = _prim "LOG_MAIL" : facility;
val NEWS = _prim "LOG_NEWS" : facility;
val SYSLOG = _prim "LOG_SYSLOG" : facility;
val USER = _prim "LOG_USER" : facility;
val UUCP = _prim "LOG_UUCP" : facility;

type loglevel = int

val EMERG = _prim "LOG_EMERG" : loglevel;
val ALERT = _prim "LOG_ALERT" : loglevel;
val CRIT = _prim "LOG_CRIT" : loglevel;
val ERR = _prim "LOG_ERR" : loglevel;
val WARNING = _prim "LOG_WARNING" : loglevel;
val NOTICE = _prim "LOG_NOTICE" : loglevel;
val INFO = _prim "LOG_INFO" : loglevel;
val DEBUG = _prim "LOG_DEBUG" : loglevel;

fun zt s = s ^ "\000"

(* openlog seems to rely on the string being around forever,
 * so I use strdup to make a copy.
 * This is a little dirty, sorry. (Personally I think it is
 * openlog's fault.)
 *)
fun openlog (s, opt, fac) =
   let 
      val optf = 
	 Word32.toInt (foldl Word32.orb 0w0 (map Word32.fromInt opt))
      val sys_strdup  = _ffi "strdup" : string -> word ;
      val sys_openlog = _ffi "openlog" : word * int * int -> unit ;
   in
      sys_openlog (sys_strdup (zt s), optf, fac)
   end

fun closelog () =
   let val sys_closelog = _ffi "closelog" : unit -> unit ;
   in sys_closelog ()
   end

fun log (lev, msg) =
   let val sys_syslog = _ffi "syslog" : int * string * string -> unit ;
   in sys_syslog (lev, "%s\000", zt msg)
   end

end
