(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature RESTORE_STRUCTS = 
   sig
      include SHRINK
   end

signature RESTORE = 
   sig
      include RESTORE_STRUCTS

      val restoreFunction: Statement.t vector -> Function.t -> Function.t
(*      val restoreProgram: Program.t -> Program.t *)
   end
