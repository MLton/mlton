(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature LIMIT_CHECK_STRUCTS = 
   sig
      include CPS
   end

signature LIMIT_CHECK = 
   sig
      include LIMIT_CHECK_STRUCTS

      (* No means definitely do not insert a limit check at this point
       * Maybe means only insert one if bytesAllocated > 0
       * Yes means insert one no matter what.  This is to check for signals.
       *)
      datatype t = No | Maybe | Yes

      (* Determine at which jumps limit checks should be inserted. *)
      val limitCheck: Program.t -> {destroy: unit -> unit,
				    limitCheck: Jump.t -> t}
   end
