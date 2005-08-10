(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature GLOBALIZE_STRUCTS = 
   sig
      include SXML
   end

signature GLOBALIZE = 
   sig
      include GLOBALIZE_STRUCTS
      
      val globalize: {
		      program: Program.t,
		      lambdaFree: Lambda.t -> Var.t vector,
		      varGlobal: Var.t -> bool ref
		     } -> unit
   end
