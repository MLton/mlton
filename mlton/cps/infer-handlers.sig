(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature INFER_HANDLERS_STRUCTS = 
   sig
      include ANALYZE
   end

signature INFER_HANDLERS = 
   sig
      include INFER_HANDLERS_STRUCTS
      
      val deltaHandlers: Dec.t * Jump.t list -> Jump.t list
      val inferHandlers: Program.t -> Jump.t -> Jump.t list
   end
