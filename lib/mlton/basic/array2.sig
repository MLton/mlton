(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t
   
signature ARRAY2 =
   sig
      type 'a t

      val copy: 'a t -> 'a t
      val equals: 'a t * 'a t * ('a * 'a -> bool) -> bool
      val forall: 'a t * ('a -> bool) -> bool
      val foreach: 'a t * ('a -> unit) -> unit
      val foreachi: 'a t * (int * int * 'a -> unit) -> unit
      val fromList: 'a list list -> 'a t
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val nCols: 'a t -> int
      val nRows: 'a t -> int
      val new: int * int * 'a -> 'a t
      val sub: 'a t * int * int -> 'a 
      val tabulate: int * int * (int * int -> 'a) -> 'a t
      val update: 'a t * int * int * 'a -> unit 
   end
