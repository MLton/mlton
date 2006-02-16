(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Pervasive.Int.int
   
signature STREAM_STRUCTS = 
   sig
   end

signature STREAM = 
   sig
      include STREAM_STRUCTS
      
      type 'a t

      val append: 'a t * 'a t -> 'a t
      val appendMap: 'a t * ('a -> 'b t) -> 'b t
      val cons: 'a * 'a t -> 'a t
      val delay: (unit -> 'a t) -> 'a t
      val empty: unit -> 'a t
      val firstN: 'a t * int -> 'a list
      val force: 'a t -> ('a * 'a t) option
      val infinite: 'a * ('a -> 'a) -> 'a t
      val isEmpty: 'a t -> bool
      val keep: 'a t * ('a -> bool) -> 'a t
      val last: 'a t -> 'a option
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val map: 'a t * ('a -> 'b) -> 'b t
      val nth: 'a t * int -> 'a
      val single: 'a -> 'a t
      val toList: 'a t -> 'a list
   end
