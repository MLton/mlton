(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure CommandLine: COMMAND_LINE =
   struct
      structure Prim = Primitive.CommandLine
	 
      fun name () = C.CS.toString (Prim.commandName ())

      fun arguments () =
	 Array.toList (C.CSS.toArrayOfLength (Prim.argv (), Prim.argc ()))
   end
