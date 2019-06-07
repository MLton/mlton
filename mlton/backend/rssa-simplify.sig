(* Copyright (C) 2019 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RSSA_SIMPLIFY_STRUCTS =
   sig
      include RSSA_RESTORE
   end

signature RSSA_SIMPLIFY =
   sig
      include RSSA_SIMPLIFY_STRUCTS

      val simplify: Program.t -> Program.t
   end
