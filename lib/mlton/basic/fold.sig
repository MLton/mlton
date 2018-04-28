(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature FOLD_STRUCTS = 
   sig
      type 'a t
      type 'a elt

      val fold: 'a t * 'b * ('a elt * 'b -> 'b) -> 'b
   end

signature FOLD = 
   sig
      include FOLD_STRUCTS

      val foldi: 'a t * 'b * (int * 'a elt * 'b -> 'b) -> 'b
      val foreachi: 'a t * (int * 'a elt -> unit) -> unit
      val foreach: 'a t * ('a elt -> unit) -> unit
      (* keepAll (l, f) keeps all x in l such that f x. *)
      val keepAll: 'a t * ('a elt -> bool) -> 'a elt list
      (* keepAllMap (l, f) keeps all y in l such that f x = SOME y.*)
      val keepAllMap: 'a t * ('a elt -> 'b option) -> 'b list
      val last: 'a t -> 'a elt
      val layout: ('a elt -> Layout.t) -> 'a t -> Layout.t
      val length: 'a t -> int
      val map: 'a t * ('a elt -> 'b) -> 'b list
      val mapi: 'a t * (int * 'a elt -> 'b) -> 'b list
      (* removeAll (l, f) removes all x in l such that f x. *)
      val removeAll: 'a t * ('a elt -> bool) -> 'a elt list
      (* The "rev" versions of functions are there for efficiency, when it is
       * easier to fold over the input and accumulate the result in reverse.
       *)
      val revKeepAll: 'a t * ('a elt -> bool) -> 'a elt list
      val revKeepAllMap: 'a t * ('a elt -> 'b option) -> 'b list
      val revRemoveAll: 'a t * ('a elt -> bool) -> 'a elt list
   end
