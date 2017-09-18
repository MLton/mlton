(* Copyright (C) 2017 James Reilly.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PARSE_SSA_STRUCTS =
   sig
      structure SsaTree: SSA_TREE 
   end

signature PARSE_SSA =
   sig
      include PARSE_SSA_STRUCTS

      val parse: char Stream.t -> SsaTree.Program.t
   end
