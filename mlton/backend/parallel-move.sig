(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PARALLEL_MOVE_STRUCTS = 
   sig
   end

signature PARALLEL_MOVE = 
   sig
      include PARALLEL_MOVE_STRUCTS

      (* Allows overlapping froms and tos.
       * Hence, has to be careful to use
       * additional working temporaries .
       *)
      val move:
         {
          (* Are two temporaries the same. *)
          equals: 'temporary * 'temporary -> bool,
          (* How to create a move statement. *)
          move: {src: 'temporary, dst: 'temporary} -> 'statement,
          (* The moves to occur. *)
          moves: {src: 'temporary, dst: 'temporary} list,
          (* Would writing the write invalidate the read? *)
          interfere: 'temporary * 'temporary -> bool,
          (* Return a new temporary like input temporary. *)
          temp: 'temporary -> 'temporary
         } -> 'statement list
   end
