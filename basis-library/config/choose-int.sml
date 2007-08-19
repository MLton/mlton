(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
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

signature CHOOSE_INT_ARG =
   sig
      type 'a t
      val fInt8: Int8.int t
      val fInt16: Int16.int t
      val fInt32: Int32.int t
      val fInt64: Int64.int t
      val fIntInf: IntInf.int t
   end

functor ChooseInt_Int8 (A : CHOOSE_INT_ARG) : 
   sig val f : Int8.int A.t end = 
   struct val f = A.fInt8 end
functor ChooseInt_Int16 (A : CHOOSE_INT_ARG) : 
   sig val f : Int16.int A.t end = 
   struct val f = A.fInt16 end
functor ChooseInt_Int32 (A : CHOOSE_INT_ARG) : 
   sig val f : Int32.int A.t end = 
   struct val f = A.fInt32 end
functor ChooseInt_Int64 (A : CHOOSE_INT_ARG) : 
   sig val f : Int64.int A.t end = 
   struct val f = A.fInt64 end
functor ChooseInt_IntInf (A : CHOOSE_INT_ARG) : 
   sig val f : IntInf.int A.t end = 
   struct val f = A.fIntInf end
