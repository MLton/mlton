(* Copyright (C) 2003-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
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
