(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature PARALLEL_MOVE_STRUCTS = 
   sig
   end

signature PARALLEL_MOVE = 
   sig
      include PARALLEL_MOVE_STRUCTS

      (* Allows overlapping froms and tos.
       * Hence, has to be careful to use temps.
       *)
      val move:
	 {
	  (* Are two registers the same. *)
	  equals: 'register * 'register -> bool,
	  (* How to create a move statement. *)
	  move: {src: 'register, dst: 'register} -> 'statement,
	  (* The moves to occur. *)
	  moves: {src: 'register, dst: 'register} list,
	  (* Would writing the write invalidate the read? *)
	  interfere: 'register * 'register -> bool,
	  (* Return a new temporary register like input register. *)
	  temp: 'register -> 'register
	 } -> 'statement list
   end
