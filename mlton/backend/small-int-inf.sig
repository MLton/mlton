(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature SMALL_INT_INF_STRUCTS = 
   sig
   end

signature SMALL_INT_INF = 
   sig
      include SMALL_INT_INF_STRUCTS
      
      type t

      val toCstring: t -> string
      val toMLstring: t -> string
      val toWord: t -> Word.t
   end
