(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure NullString =
   struct
      open Primitive.NullString8

      val nullTerm = fromString o String.nullTerm
   end
structure NullStringArray =
   struct
      open Primitive.NullString8Array
   end
