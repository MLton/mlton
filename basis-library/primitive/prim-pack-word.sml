(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

structure Primitive = struct

open Primitive

structure PackWord8 =
   struct
      type word = Word8.word

      val subArr = 
         _prim "Word8Array_subWord8": Word8.word array * SeqIndex.int -> word;
      val subVec = 
         _prim "Word8Vector_subWord8": Word8.word vector * SeqIndex.int -> word;
      val update = 
         _prim "Word8Array_updateWord8": Word8.word array * SeqIndex.int * word -> unit;
   end

structure PackWord16 =
   struct
      type word = Word16.word

      val subArr = 
         _prim "Word8Array_subWord16": Word8.word array * SeqIndex.int -> word;
      val subVec = 
         _prim "Word8Vector_subWord16": Word8.word vector * SeqIndex.int -> word;
      val update = 
         _prim "Word8Array_updateWord16": Word8.word array * SeqIndex.int * word -> unit;
   end

structure PackWord32 =
   struct
      type word = Word32.word

      val subArr = 
         _prim "Word8Array_subWord32": Word8.word array * SeqIndex.int -> word;
      val subVec = 
         _prim "Word8Vector_subWord32": Word8.word vector * SeqIndex.int -> word;
      val update = 
         _prim "Word8Array_updateWord32": Word8.word array * SeqIndex.int * word -> unit;
   end

structure PackWord64 =
   struct
      type word = Word64.word

      val subArr = 
         _prim "Word8Array_subWord64": Word8.word array * SeqIndex.int -> word;
      val subVec = 
         _prim "Word8Vector_subWord64": Word8.word vector * SeqIndex.int -> word;
      val update = 
         _prim "Word8Array_updateWord64": Word8.word array * SeqIndex.int * word -> unit;
   end

end
