(* Copyright (C) 2003-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature DEFUNCTORIZE_STRUCTS = 
   sig
      structure CoreML: CORE_ML
      structure Xml: XML
      sharing CoreML.Atoms = Xml.Atoms
   end

signature DEFUNCTORIZE = 
   sig
      include DEFUNCTORIZE_STRUCTS

      val defunctorize: CoreML.Program.t -> Xml.Program.t
   end
