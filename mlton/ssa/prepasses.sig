(* Copyright (C) 2005-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PREPASSES_STRUCTS = 
   sig
      include TYPE_CHECK
   end

signature PREPASSES = 
   sig
      include PREPASSES_STRUCTS

      (* A critical edge is one that connects a block with two or more
       * succesors to one with two or more predecessors.
       * This prepass breaks all critical edges by inserting an eta-block.
       * For some analyses and transformations, simply ensuring the unique
       * successor or predecessor property is sufficient.  (For example, see
       * the comments at the end of "Conditional Constant Propagation" in
       * Section 19.3 of Appel's "Modern Compiler Implementation in ML".) 
       * However, passes that require critical edges to be broken in order
       * to accomodate code motion (for example, PRE), should also break an
       * edge that connects a block with non-functional control transfer to
       * one with two or more predecessors.
       *)
      val breakCriticalEdgesFunction: 
         Function.t * {codeMotion: bool} -> Function.t
      val breakCriticalEdges: 
         Program.t * {codeMotion: bool} -> Program.t
      val eliminateDeadBlocksFunction: Function.t -> Function.t
      val eliminateDeadBlocks: Program.t -> Program.t
      val orderFunctions: Program.t -> Program.t
      val reverseFunctions: Program.t -> Program.t
   end
