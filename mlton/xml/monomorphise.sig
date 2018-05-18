(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
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
