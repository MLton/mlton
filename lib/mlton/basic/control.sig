(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CONTROL =
   sig
      val all: unit -> {name: string,
                        value: string} list
      val control: {name: string,
                    default: 'a,
                    toString: 'a -> string} -> 'a ref
      val setDefaults: unit -> unit
   end
