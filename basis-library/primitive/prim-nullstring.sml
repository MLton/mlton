(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

structure Primitive  = struct

open Primitive

(* NullString is used for strings that must be passed to C and hence must be
 * null terminated.
 *)
structure NullString8 :>
   sig
      type t

      val empty: t
      val fromString: String8.string -> t
   end =
   struct
      type t = String8.string

      fun fromString s =
         if #"\000" = Vector.subUnsafe (s, SeqIndex.- (Vector.length s, 1))
            then s
         else raise Exn.Fail8 "NullString.fromString"

      val empty = fromString "\000"
   end
structure NullString8Array = struct type t = NullString8.t array end

end
