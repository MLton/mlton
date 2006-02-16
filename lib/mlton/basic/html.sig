(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t
   
signature HTML = 
   sig
      structure Align:
         sig
            datatype t = Left | Center | Right
         end
      structure Element:
         sig
            type t

            datatype tableOption =
               Border of int
             | CellPadding of int
             | CellSpacing of int

            val a: Url.t * t -> t
            val br: t
            val img: {src: Url.t} -> t
            val layout: t -> Layout.t
            val pre: t -> t
            val seq: t list -> t
            val str: string -> t
            val tt: t -> t
            val table: tableOption list * t list list -> t
         end
      structure Option:
         sig
            datatype t =
               Redirect of {seconds: int,
                            uri: Url.t}
             | Title of string
         end

      datatype t = T of {options: Option.t list,
                         body: Element.t}
      val layout: t -> Layout.t
   end
