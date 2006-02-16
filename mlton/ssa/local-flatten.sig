(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LOCAL_FLATTEN_STRUCTS = 
   sig
      include SHRINK
   end

signature LOCAL_FLATTEN = 
   sig
      include LOCAL_FLATTEN_STRUCTS

      (* Intraprocedural flattening. *)
      val flatten: Program.t -> Program.t
   end
