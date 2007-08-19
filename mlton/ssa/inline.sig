(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t

signature INLINE_STRUCTS = 
   sig
      include SHRINK
   end

signature INLINE = 
   sig
      include INLINE_STRUCTS

      val inlineLeaf: 
         Program.t * {loops: bool, repeat: bool, size: int option} -> Program.t
      val inlineNonRecursive: 
         Program.t * {small:int,product:int} -> Program.t
   end
