(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature XML_STRUCTS =
   sig
      include XML_TREE_STRUCTS
   end

signature XML =
   sig
      include XML_TREE

      val simplify: Program.t -> Program.t
      val typeCheck: Program.t -> unit
   end
