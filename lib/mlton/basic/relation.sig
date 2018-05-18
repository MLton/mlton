(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RELATION =
   sig
      datatype t = datatype order (* from the pervasive environment *)

      val compare: ('a * 'a -> t)
         -> {equals: 'a * 'a -> bool,
             < : 'a * 'a -> bool,
             > : 'a * 'a -> bool,
             >= : 'a * 'a -> bool,
             <= : 'a * 'a -> bool,
             min: 'a * 'a -> 'a,
             max: 'a * 'a -> 'a}
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val lexico: t * (unit -> t) -> t
      val lessEqual: {< : 'a * 'a -> bool,
                       equals: 'a * 'a -> bool}
         -> {> : 'a * 'a -> bool,
             >= : 'a * 'a -> bool,
             <= : 'a * 'a -> bool,
             min: 'a * 'a -> 'a,
             max: 'a * 'a -> 'a,
             compare: 'a * 'a -> t}
   end
