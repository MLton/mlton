(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)

signature UNUSED_ARGS_STRUCTS = 
   sig
      include SHRINK
   end

signature UNUSED_ARGS = 
   sig
      include UNUSED_ARGS_STRUCTS

       (* Intraprocedural analysis.
	*  - removed usused args from jumps
	*  - insert wrappers around jumps with unused args
	*     that are conts, handlers, or constructor switch cases
	*  - remove unused bindings
	*)
      val unusedArgs: Program.t -> Program.t
   end
