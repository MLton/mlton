(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SUBSTRING =
   sig
      type t

      val base: t -> string * {start: int, length: int}
      val compare: t * t -> order 
      val concat: t list -> string
      val endOf: t -> int (* start + length *)
      val explode: t -> char list 
      val extract: string * int * int option -> t 
      val first: t -> char option
      val full: string -> t
      val getc: t -> (char * t) option 
      val isEmpty: t -> bool
      val layout: t -> Layout.t
      val length: t -> int 
      val slice: t * int * int option -> t
      val span: t * t -> t
      val string: t -> string 
      val sub: t * int -> char 
      val substring: string * {start: int, length: int} -> t
      val toString: t -> string
   end
