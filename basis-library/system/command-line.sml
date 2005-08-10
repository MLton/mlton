(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure CommandLine: COMMAND_LINE =
   struct
      structure Prim = Primitive.CommandLine
	 
      fun name () = C.CS.toString (Prim.commandName ())

      fun arguments () =
	 Array.toList (C.CSS.toArrayOfLength (Prim.argv (), Prim.argc ()))
   end
