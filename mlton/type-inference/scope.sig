(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
