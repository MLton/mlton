(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Pervasive.Int.int
      
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
      val keepAll: 'a t * ('a elt -> bool) -> 'a elt list
      val keepAllMap: 'a t * ('a elt -> 'b option) -> 'b list
      val last: 'a t -> 'a elt
      val layout: ('a elt -> Layout.t) -> 'a t -> Layout.t
      val length: 'a t -> int
      val map: 'a t * ('a elt -> 'b) -> 'b list
      val mapi: 'a t * (int * 'a elt -> 'b) -> 'b list
      val removeAll: 'a t * ('a elt -> bool) -> 'a elt list
      val subset: 'a t * ('a elt -> bool) -> 'a elt list
   end
