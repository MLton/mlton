(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure CommandLine: COMMAND_LINE =
   struct
      structure Prim = PrimitiveFFI.CommandLine

      fun name () = 
         CUtil.C_String.toString (Prim.commandNameGet ())

      fun arguments () =
         (Array.toList o CUtil.C_StringArray.toArrayOfLength) 
         (Prim.argvGet (), C_Int.toInt (Prim.argcGet ()))
   end
