(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure CommandLine: COMMAND_LINE =
   struct
      structure Prim = Primitive.CommandLine
         
      fun name () = C.CS.toString (Prim.commandName ())

      fun arguments () =
         Array.toList (C.CSS.toArrayOfLength (Prim.argv (), Prim.argc ()))
   end
