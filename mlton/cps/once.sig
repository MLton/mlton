(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature ONCE_STRUCTS = 
   sig
      include INFER_HANDLERS
   end

signature ONCE = 
   sig
      include ONCE_STRUCTS

      (* Returns true for let bound variables that are guaranteed to
       * be bound no more than once during the entire program.
       *)
      val once: Program.t -> Var.t -> bool
   end
