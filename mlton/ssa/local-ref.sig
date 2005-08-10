(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
