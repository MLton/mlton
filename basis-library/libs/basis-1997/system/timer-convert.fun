(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
