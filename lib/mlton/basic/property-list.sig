(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PROPERTY_LIST = 
   sig
      type t

      (* remove all properties from the list *)
      val clear: t -> unit
      (* pointer equality of property lists *)
      val equals: t * t -> bool
      val length: t -> int
      (* create an empty property list *)
      val new: unit -> t
      (* create a new property *)
      val newProperty:
         unit -> {
                  (* See if a property is in a property list.
                   * NONE if it isn't.
                   *)
                  peek: t -> 'a option,
                  (* Add the value of the property -- must not already exist. *)
                  add: t * 'a -> unit,
                  (* Remove a property from a property list.
                   * Noop if the property isn't there.
                   *)
                  remove: t -> unit
                  }
      val stats: unit -> Layout.t
   end
