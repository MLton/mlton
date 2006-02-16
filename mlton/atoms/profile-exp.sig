(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type word = Word.t
   
signature PROFILE_EXP_STRUCTS =
   sig
      structure SourceInfo: SOURCE_INFO
   end

signature PROFILE_EXP =
   sig
      include PROFILE_EXP_STRUCTS

      datatype t =
         Enter of SourceInfo.t
       | Leave of SourceInfo.t

      val equals: t * t -> bool
      val hash: t -> word
      val layout: t -> Layout.t
      val toString: t -> string
   end
