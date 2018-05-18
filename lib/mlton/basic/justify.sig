(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature JUSTIFY =
   sig
      datatype t =
         Left
       | Center
       | Right

      val justify: string * int * t -> string
      val outputTable: string list list * Out.t -> unit
      val table: {columnHeads: string list option,
                  justs: t list,
                  rows: string list list} -> string list list
      val tableOfColumns: (t * string list) list -> string list list
   end
