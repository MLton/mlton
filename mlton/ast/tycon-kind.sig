(* Copyright (C) 2003-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t

signature TYCON_KIND_STRUCTS = 
   sig
   end

signature TYCON_KIND = 
   sig
      include TYCON_KIND_STRUCTS

      datatype t =
         Arity of int
       | Nary

      val equals: t * t -> bool
      val layout: t -> Layout.t
   end
