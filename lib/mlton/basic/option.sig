(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature OPTION =
   sig
      type 'a t = 'a option

      val app: 'a t * ('a -> unit) -> unit
      val equals: 'a t * 'a t * ('a * 'a -> bool) -> bool
      val exists: 'a t * ('a -> bool) -> bool
      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val forall: 'a t * ('a -> bool) -> bool
      val foreach: 'a t * ('a -> unit) -> unit
      val isNone: 'a t -> bool
      val isSome: 'a t -> bool
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val map: 'a t * ('a -> 'b) -> 'b t
      val toString: ('a -> string) -> 'a t -> string
      val valOf: 'a t -> 'a
   end
