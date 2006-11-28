(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

structure Primitive = struct

open Primitive

structure String8 =
   struct
      open String8

      val idFromWord8Vector = 
         _prim "Word8Vector_toString": Word8.word vector -> string;
      val idToWord8Vector = 
         _prim "String_toWord8Vector": string -> Word8.word vector;
   end

end
