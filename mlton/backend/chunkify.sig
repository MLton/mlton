(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature CHUNKIFY_STRUCTS = 
   sig
      include CPS
   end

signature CHUNKIFY = 
   sig
      include CHUNKIFY_STRUCTS

      (* Partitions all the labels declared into disjoint sets, referred
       * to as chunks.  Returns the list of chunks.
       * All funcs, conts, and handlers are assumed to be entry points.
       * All conts and handlers are assumed to be return points.
       *)
      val chunkify:
	 {program: Program.t,
	  jumpHandlers: Jump.t -> Jump.t list}
	 -> {
	     funcs: Func.t list,
	     jumps: Jump.t list
	     } list
   end
