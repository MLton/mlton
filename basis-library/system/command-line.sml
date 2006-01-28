(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
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
         COld.CS.toString 
         (Primitive.Pointer.fromWord (Prim.commandNameGet ()))

      fun arguments () =
         (Array.toList o COld.CSS.toArrayOfLength) 
         (Primitive.Pointer.fromWord (Prim.argvGet ()), Prim.argcGet ())
   end
