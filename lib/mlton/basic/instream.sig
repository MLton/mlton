(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INSTREAM =
   sig
      type t

      val close: t -> unit
      val endOf: t -> bool
      val foldChars: t * 'a * (char * 'a -> 'a) -> 'a
      (* Each line includes the newline. *)
      val foldLines: t * 'a * (string * 'a -> 'a) -> 'a
      val foreachLine: t * (string -> unit) -> unit
      val ignoreSpaces: t -> unit
      val input: t -> string
      val inputAll: t -> string
      val inputChar: t -> char option
      val inputLine: t -> string option
      val inputN: t * int -> string
      val inputNothing: t -> unit
      (* inputTo(i, p) inputs up to but not including the first char
       * that satisfies p.
       *)
      val inputTo: t * (char -> bool) -> string
      val inputToChar: t * char -> string
      val inputToSpace: t -> string
      val layout: t -> Layout.t
      (* Each line includes the newline. *)
      val lines: t -> string list
      val openIn: string -> t    
      val openString: string -> t
      val outputAll: t * Out.t -> unit
      val peekChar: t -> char option
      val sameContents: t * t -> bool
(*      val set: t * t -> unit *)
      val standard: t
      val withClose: t * (t -> 'a) -> 'a
      val withNull: (t -> 'a) -> 'a
   end
