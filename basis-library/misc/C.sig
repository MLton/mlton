(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature C =
   sig
      (* C char* *)
      structure CS :
         sig
            type t

            (* string must be null terminated *)
            val length: t -> int
            val sub: t * int -> char
            val toCharArrayOfLength: t * int -> char array
            (* string must be null terminated *)
            val toString: t -> string
            (* extract first n characters of string *)
            val toStringOfLength: t * int -> string
            val update: t * int * char -> unit
         end

      (* NULL terminated char** *)
      structure CSS:
         sig
            type t

            val fromList: string list -> NullString.t array
            (* extract first n strings from array *)
            val toArrayOfLength: t * int -> string array
            val toList: t -> string list
         end
   end
