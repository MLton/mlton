(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RECORD_STRUCTS = 
   sig
      val isSorted: bool
      structure Field: FIELD
   end

signature RECORD = 
   sig
      include RECORD_STRUCTS

      type 'a t

      (* Create a record with the same fields but a new range.
       * Also return some additional info.
       *)
      val change: 'a t * ('a vector -> 'b vector * 'c) -> 'b t * 'c
      (* detuple r returns the components, if r is a tuple *)
      val detupleOpt: 'a t -> 'a vector option
      val exists: 'a t * ('a -> bool) -> bool
      val forall: 'a t * ('a -> bool) -> bool
      val foreach: 'a t * ('a -> unit) -> unit
      val fromVector: (Field.t * 'a) vector -> 'a t
      val layout: {record: 'a t,
                   separator: string,
                   extra: string,
                   layoutTuple: 'a vector -> Layout.t,
                   layoutElt: 'a -> Layout.t} -> Layout.t
      val map: 'a t * ('a -> 'b) -> 'b t
      val peek: 'a t * Field.t -> 'a option
      (* range {1 = a, 2 = b, 3 = c} returns [a, b, c] *)
      val range: 'a t -> 'a vector
      val toVector: 'a t -> (Field.t * 'a) vector
      (* tuple [a, b, c] creates {1 = a, 2 = b, 3 = c} *)
      val tuple: 'a vector -> 'a t
      val zip: Field.t vector * 'a vector -> 'a t
   end
