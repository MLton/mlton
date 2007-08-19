(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type word = Word.t

signature GLOBAL_STRUCTS = 
   sig
      include SSA_TREE
   end

signature GLOBAL = 
   sig
      include GLOBAL_STRUCTS

      val make:
         unit -> {
                  new: Type.t * Exp.t -> Var.t,
                  all: unit -> Statement.t vector
                 }
   end
