(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REF =
   sig
      type 'a t

      val ! : 'a t -> 'a
      val := : 'a t * 'a -> unit

      val equals: 'a t * 'a t -> bool
      val fluidLet: 'a t * 'a * (unit -> 'b) -> 'b
      val getAndSet: ('a -> 'b ref) -> ('a -> 'b) * ('a * 'b -> unit)
      val getSet: ('a -> Layout.t) -> {get: unit -> 'a,
                                        set: 'a -> unit,
                                        clear: unit -> unit,
                                        layout: unit -> Layout.t,
                                        output: Out.t -> unit,
                                        print: unit -> unit}
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val memoize: 'a option t * (unit -> 'a) -> 'a
      val swap: 'a t * 'a t -> unit
   end
