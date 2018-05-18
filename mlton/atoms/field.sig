(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature FIELD_STRUCTS = 
   sig
      structure Symbol: SYMBOL
   end

signature FIELD = 
   sig
      include FIELD_STRUCTS

      datatype t =
         Int of int
       | Symbol of Symbol.t

      val <= : t * t -> bool (* ordering used for sorting *)
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val toString: t -> string
   end
