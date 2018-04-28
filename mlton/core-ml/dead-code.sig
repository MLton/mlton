(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature DEAD_CODE_STRUCTS = 
   sig
      structure CoreML: CORE_ML
   end

signature DEAD_CODE = 
   sig
      include DEAD_CODE_STRUCTS

      val deadCode:
         {prog: (CoreML.Dec.t list * bool) vector} ->
         {prog: CoreML.Dec.t list vector}
   end
