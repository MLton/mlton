(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature IMPLEMENT_HANDLERS_STRUCTS = 
   sig
      structure Ssa: SSA
   end

signature IMPLEMENT_HANDLERS = 
   sig
      include IMPLEMENT_HANDLERS_STRUCTS
      
      val doit: Ssa.Program.t -> Ssa.Program.t
   end
