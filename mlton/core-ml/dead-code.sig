(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
