(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
