(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature RAISE_TO_JUMP_STRUCTS = 
   sig
      include SHRINK
   end

signature RAISE_TO_JUMP = 
   sig
      include RAISE_TO_JUMP_STRUCTS

       (* Translate raises where the handler is known into jumps.
	* Install and remove handlers only around nontail calls.
	* Remove all other handler pushes and pops.
	*)
      val raiseToJump: Program.t -> Program.t
   end
