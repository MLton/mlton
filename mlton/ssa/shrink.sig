(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature SHRINK_STRUCTS = 
   sig
      include TYPE_CHECK
   end

signature SHRINK = 
   sig
      include SHRINK_STRUCTS

      val shrinkFunction: Statement.t vector -> Function.t -> Function.t
      val shrink: Program.t -> Program.t
   end
