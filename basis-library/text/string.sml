(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure String1 = StringFn(Char1Vector)
structure String2 = StringFn(Char2Vector)
structure String4 = StringFn(Char4Vector)

structure String = String1
structure WideString = String4

structure StringGlobal: STRING_GLOBAL = String
open StringGlobal

(* Now that concat is defined, we can add the exnMessager for Fail. *)
val _ =
   General.addExnMessager
   (fn e =>
    case e of
       Fail s => SOME (concat ["Fail: ", s])
     | _ => NONE)

structure NullString =
   struct
      open NullString

      val nullTerm = fromString o String.nullTerm
   end
