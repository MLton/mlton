(* Copyright (C) 2019 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RSSA_STRUCTS = RSSA_TREE_STRUCTS

signature RSSA =
   sig
      include RSSA_SIMPLIFY
   end
