(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature SCOPE_STRUCTS =
   sig
      structure CoreML: CORE_ML
   end

signature SCOPE =
   sig
      include SCOPE_STRUCTS

      (* add free type variables to the val or fun declaration
       where they are implicitly scoped *)
      val scope: CoreML.Program.t -> CoreML.Program.t
   end
