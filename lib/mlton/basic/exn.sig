(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature EXN =
   sig
      type t = exn

      exception Bind
      exception Match
      exception Overflow
      exception Subscript

      val finally: (unit -> 'a) * (unit -> unit) -> 'a
      val history: t -> string list
      val name: t -> string
      val layout: t -> Layout.t
      val toString: t -> string
      (* try (t, k, h) evaluates t (), and if it yields value v, evaluates k v.
       * If t () raises exception e, then h e is evaluated.
       * This is not the same as "k (t ()) handle e => h e", because it doesn't
       * evaluate k v in the context of the handler.  See "Exceptional Syntax"
       * by Benton and Kennedy. 
       *)
      val try: (unit -> 'a) * ('a -> 'b) * (t -> 'b) -> 'b
      val withEscape: (('a -> 'b) -> 'a) -> 'a
   end
