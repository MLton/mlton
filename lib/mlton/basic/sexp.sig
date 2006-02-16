(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SEXP_STRUCTS = 
   sig
   end

signature SEXP = 
   sig
      include SEXP_STRUCTS
      
      datatype t =
         Atom of string
       | List of t list
       | String of string

      datatype parseResult =
         Eof
       | Error of string
       | Sexp of t

      val fromString: string -> parseResult
      val input: In.t -> parseResult
      val layout: t -> Layout.t
      val toString: t -> string
   end
