(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature IMPLEMENT_EXCEPTIONS_STRUCTS = 
   sig
      include SXML_TREE
   end

signature IMPLEMENT_EXCEPTIONS = 
   sig
      include IMPLEMENT_EXCEPTIONS_STRUCTS
      
      val doit: Program.t -> Program.t
   end
