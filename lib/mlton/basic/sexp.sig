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

      val input: In.t -> t option (* NONE if eof *)
      val layout: t -> Layout.t
   end
