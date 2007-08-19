(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHOOSE_WORDN_ARG =
   sig
      type 'a t
      val fWord8: Word8.word t
      val fWord16: Word16.word t
      val fWord32: Word32.word t
      val fWord64: Word64.word t
   end

functor ChooseWordN_Word8 (A : CHOOSE_WORDN_ARG) : 
   sig val f : Word8.word A.t end = 
   struct val f = A.fWord8 end
functor ChooseWordN_Word16 (A : CHOOSE_WORDN_ARG) : 
   sig val f : Word16.word A.t end = 
   struct val f = A.fWord16 end
functor ChooseWordN_Word32 (A : CHOOSE_WORDN_ARG) : 
   sig val f : Word32.word A.t end = 
   struct val f = A.fWord32 end
functor ChooseWordN_Word64 (A : CHOOSE_WORDN_ARG) : 
   sig val f : Word64.word A.t end = 
   struct val f = A.fWord64 end
