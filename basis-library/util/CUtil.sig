(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature C_UTIL =
   sig
      structure C_Pointer :
         sig
            type t = C_Pointer.t

            val null: t
            val isNull: t -> bool
         end

      (* C char* *)
      structure C_String :
         sig
            type t = C_String.t

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
      structure C_StringArray :
         sig
            type t = C_StringArray.t

            (* extract first n strings from array *)
            val toArrayOfLength: t * int -> string array
            val toList: t -> string list
         end

      (* NULL terminated char** *)
      structure StringVector :
         sig
            type t = string * C_Pointer.t array * C_Size.t vector
            val fromList: string list -> t
         end
   end
