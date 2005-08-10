(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t

signature CHUNKIFY_STRUCTS = 
   sig
      include RSSA
   end

signature CHUNKIFY = 
   sig
      include CHUNKIFY_STRUCTS

      (* Partitions all the labels declared into disjoint sets, referred
       * to as chunks.  Returns the list of chunks.
       * All funcs, conts, and handlers are assumed to be entry points.
       * All conts and handlers are assumed to be return points.
       *)
      val chunkify: Program.t -> {
				  funcs: Func.t vector,
				  labels: Label.t vector
				  } vector
   end
