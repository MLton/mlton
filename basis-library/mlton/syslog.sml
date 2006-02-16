(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
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

open Primitive.MLton.Syslog

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
      val sys_strdup  = _import "strdup" : string -> word ;
      val sys_openlog = _import "openlog" : word * int * int -> unit ;
   in
      sys_openlog (sys_strdup (zt s), optf, fac)
   end

fun closelog () =
   let val sys_closelog = _import "closelog" : unit -> unit ;
   in sys_closelog ()
   end

fun log (lev, msg) =
   let val sys_syslog = _import "syslog" : int * string * string -> unit ;
   in sys_syslog (lev, "%s\000", zt msg)
   end

end
