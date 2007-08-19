(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type word = Word.t

signature REAL_X_STRUCTS = 
   sig
      structure RealSize: REAL_SIZE
   end

signature REAL_X = 
   sig
      include REAL_X_STRUCTS

      (* reals of all RealSize.t sizes. *)
      type t

      val equals: t * t -> bool
      val hash: t -> word
      val layout: t -> Layout.t
      val make: string * RealSize.t -> t option
      val size: t -> RealSize.t
      val toString: t -> string
      val zero: RealSize.t -> t
   end
