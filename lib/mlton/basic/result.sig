(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RESULT =
   sig
      datatype 'a t =
         No of string
       | Yes of 'a

      val isNo: 'a t -> bool
      val isYes: 'a t -> bool
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val map: 'a t * ('a -> 'b) -> 'b t
   end
      
