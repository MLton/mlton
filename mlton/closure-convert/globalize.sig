(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
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
