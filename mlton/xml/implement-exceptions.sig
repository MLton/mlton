(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature IMPLEMENT_EXCEPTIONS_STRUCTS = 
   sig
      include SXML
   end

signature IMPLEMENT_EXCEPTIONS = 
   sig
      include IMPLEMENT_EXCEPTIONS_STRUCTS
      
      val doit: Program.t -> Program.t
   end
