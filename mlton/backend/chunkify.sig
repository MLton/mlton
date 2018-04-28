(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

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
