(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor TimerConvert
        (structure Timer: TIMER) :
        TIMER_1997 =
  struct
     open Timer

     val checkCPUTimer = fn cput =>
       let
	 val {usr, sys} = checkCPUTimer cput
	 val gc = checkGCTime cput
       in
	 {usr = usr, sys = sys, gc = gc}
       end
  end
