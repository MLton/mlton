(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature MONOMORPHISE_STRUCTS = 
   sig
      structure Xml: XML
      structure Sxml: SXML_EXNS
      sharing Xml.Atoms = Sxml.Atoms
   end

signature MONOMORPHISE = 
   sig
      include MONOMORPHISE_STRUCTS
      
      val monomorphise: Xml.Program.t -> Sxml.Program.t
   end
