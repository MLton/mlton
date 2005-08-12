(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LOCAL_REF_STRUCTS =
   sig
      include RESTORE
   end

signature LOCAL_REF =
   sig
      include LOCAL_REF_STRUCTS

      (* Local ref elimination. *)
      val eliminate: Program.t -> Program.t
   end
