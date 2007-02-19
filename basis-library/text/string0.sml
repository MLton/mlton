(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* This is the minimum needed to bootstrap StringCvt *)
structure String = 
   struct
      (* CharVector comes from mono.sml and default-charX.sml *)
      open CharVector
      type char = elem
      type string = vector
      
      val size = length
      val op ^ = append
      val implode = fromList
      val explode = toList
      val new = vector
   end

(*
structure WideString = 
   struct
      open WideCharVector
      type char = elem
      type string = vector
      
      val size = length
      val op ^ = append
      val implode = fromList
      val explode = toList
      val new = vector
   end
*)
