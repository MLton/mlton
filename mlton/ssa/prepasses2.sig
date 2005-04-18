(* Copyright (C) 2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature PREPASSES2_STRUCTS = 
   sig
      include TYPE_CHECK2
   end

signature PREPASSES2 = 
   sig
      include PREPASSES2_STRUCTS

      val eliminateDeadBlocksFunction: Function.t -> Function.t
      val eliminateDeadBlocks: Program.t -> Program.t
      val reverseFunctions: Program.t -> Program.t
   end
