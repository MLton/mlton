(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHOOSE_INTN_ARG =
   sig
      type 'a t
      val fInt8: Int8.int t
      val fInt16: Int16.int t
      val fInt32: Int32.int t
      val fInt64: Int64.int t
   end

functor ChooseIntN_Int8 (A : CHOOSE_INTN_ARG) : 
   sig val f : Int8.int A.t end = 
   struct val f = A.fInt8 end
functor ChooseIntN_Int16 (A : CHOOSE_INTN_ARG) : 
   sig val f : Int16.int A.t end = 
   struct val f = A.fInt16 end
functor ChooseIntN_Int32 (A : CHOOSE_INTN_ARG) : 
   sig val f : Int32.int A.t end = 
   struct val f = A.fInt32 end
functor ChooseIntN_Int64 (A : CHOOSE_INTN_ARG) : 
   sig val f : Int64.int A.t end = 
   struct val f = A.fInt64 end

signature CHOOSE_REALN_ARG =
   sig
      type 'a t
      val fReal32: Real32.real t
      val fReal64: Real64.real t
   end

functor ChooseRealN_Real32 (A : CHOOSE_REALN_ARG) : 
   sig val f : Real32.real A.t end = 
   struct val f = A.fReal32 end
functor ChooseRealN_Real64 (A : CHOOSE_REALN_ARG) : 
   sig val f : Real64.real A.t end = 
   struct val f = A.fReal64 end

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
