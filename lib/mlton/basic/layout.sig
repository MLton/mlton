(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LAYOUT =
   sig
      type int = Pervasive.Int.int
         
      type t
         
      (* layout the objects on separate lines*)
      val align: t list -> t
      val alignPrefix: t list * string -> t
      val array: t array -> t
      (* Whether or not to print things in detail -
       * routines that create layouts should use this flag to decide
       * how detailed to print.
       *)
      val detailed: bool ref
      val empty: t
      val ignore: 'a -> t
      val isEmpty: t -> bool
      val makeOutput: ('a -> t) -> 'a * Outstream0.t -> unit
      (* layout the objects on separate lines, if necessary *)
      val mayAlign: t list -> t
      val namedRecord: string * (string * t) list -> t
      (* indent the entire object *)
      val indent: t * int -> t
      val list: t list -> t
      val output: t * Outstream0.t -> unit
      val outputl: t * Outstream0.t -> unit
      val outputTree: t * Outstream0.t -> unit
      val outputWidth: t * int * Outstream0.t -> unit
      val paren: t -> t
      (* print the object *)
      val print: t * (string -> unit) -> unit
      val record: (string * t) list -> t
      val schemeList: t list -> t
      (* put string between elements *)
      val separate: t list * string -> t list
      (* adds string at beginning of all objects except first *)
      val separateLeft: t list * string -> t list
      (* adds string at the end of all objects except last *) 
      val separateRight: t list * string -> t list
      (* layout the objects on the same line *)
      val seq: t list -> t
      (* convert a string to a layout object *)
      val str: string -> t
      val switch: {detailed: 'a -> t, normal: 'a -> t} -> 'a -> t
      val toString: t -> string
      val tuple: t list -> t
      val tuple2: ('a -> t) * ('b -> t) -> 'a * 'b -> t
      val tuple3: ('a -> t) * ('b -> t) * ('c -> t) -> 'a * 'b * 'c -> t
      val tuple4: ('a -> t) * ('b -> t) * ('c -> t) * ('d -> t)
         -> 'a * 'b * 'c * 'd -> t
      val tuple5: ('a -> t) * ('b -> t) * ('c -> t) * ('d -> t) * ('e -> t)
         -> ('a * 'b * 'c * 'd * 'e) -> t
      val vector: t vector -> t
   end
