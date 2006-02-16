(* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PREPASSES2_STRUCTS = 
   sig
      include TYPE_CHECK2
   end

signature PREPASSES2 = 
   sig
      include PREPASSES2_STRUCTS

      val dropProfile: Program.t -> Program.t
      val eliminateDeadBlocksFunction: Function.t -> Function.t
      val eliminateDeadBlocks: Program.t -> Program.t
      val reverseFunctions: Program.t -> Program.t
   end
