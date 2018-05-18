(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature OUTSTREAM =
   sig
      type t

      val close: t -> unit
      val error: t
      val fluidLet: t * t * (unit -> 'a) -> 'a
      val flush: t -> unit
      val ignore: t * (unit -> 'a) -> 'a
      val layout: t -> Layout.t
      val newline: t -> unit
      val openAppend: string -> t
      val openOut: string -> t
      val output: t * string -> unit
      val output1: t * char -> unit
      val outputc: t -> string -> unit
      val outputl: t * string -> unit
      val outputSubstr: t * Substring.t -> unit
      val print: string -> unit
      val set: t * t -> unit
      val standard: t
      (* withClose (out, f) runs (f out) and ensures that out is closed after
       * f completes.  It is a bit redundant to pass out to f (instead of ()),
       * but it makes some code easier to write, and can be ignored if you want.
       *)
      val withClose: t * (t -> 'a) -> 'a
      val withNull: (t -> 'a) -> 'a
   end
