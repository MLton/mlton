(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t

signature CASES_STRUCTS = 
   sig
      type con
      type word

      val conEquals: con * con -> bool
      val wordEquals: word * word -> bool
   end

signature CASES = 
   sig
      include CASES_STRUCTS
      
      datatype 'a t =
         Char of (char * 'a) vector
       | Con of (con * 'a) vector
       | Int of (IntInf.t * 'a) vector
       | Word of (word * 'a) vector

      val equals: 'a t * 'a t * ('a * 'a -> bool) -> bool
      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val forall: 'a t * ('a -> bool) -> bool
      val foreach: 'a t * ('a -> unit) -> unit
      val foreach': 'a t * ('a -> unit) * (con -> unit) -> unit
      val hd: 'a t -> 'a
      val isEmpty: 'a t -> bool
      val length: 'a t -> int
      val map: 'a t * ('a -> 'b) -> 'b t
   end
