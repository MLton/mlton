(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure String = 
   struct
      open CharVector
      type string = vector
      
      val size = length
      val op ^ = append
      val implode = fromList
      val new = vector
   end

(*
structure WideString = 
   struct
      open WideCharVector
      type string = vector
      
      val size = length
      val op ^ = append
      val implode = fromList
      val new = vector
   end
*)
