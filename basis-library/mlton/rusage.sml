(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure MLtonRusage: MLTON_RUSAGE =
   struct
      structure Prim = Primitive.MLton.Rusage

      type t = {utime: Time.time, stime: Time.time}

      fun collect (utimeSec, utimeUsec, stimeSec, stimeUsec) =
	 let
	    fun toTime (sec, usec) =
	       let
		  val time_sec =
		     Time.fromSeconds (LargeInt.fromInt (sec ()))
		  val time_usec =
		     Time.fromMicroseconds (LargeInt.fromInt (usec ()))
	       in
		  Time.+ (time_sec, time_usec)
	       end
	 in
	    {stime = toTime (stimeSec, stimeUsec),
	     utime = toTime (utimeSec, utimeUsec)}
	 end

      fun rusage () =
	 let
	    val () = Prim.ru ()
	    open Prim
	 in
	    {children = collect (children_utime_sec, children_utime_usec,
				 children_stime_sec, children_stime_usec),
	     gc = collect (gc_utime_sec, gc_utime_usec,
			   gc_stime_sec, gc_stime_usec),
	     self = collect (self_utime_sec, self_utime_usec,
			     self_stime_sec, self_stime_usec)}
	 end
   end
