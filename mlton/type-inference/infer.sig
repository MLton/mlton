(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
   
signature INFER_STRUCTS = 
   sig
      structure CoreML: CORE_ML
      structure Xml: XML
      structure LookupConstant: LOOKUP_CONSTANT
      sharing CoreML.Atoms = Xml.Atoms
      sharing CoreML = LookupConstant.CoreML
   end

signature INFER = 
   sig
      include INFER_STRUCTS
      
      val infer:
	 {program: CoreML.Program.t,
	  lookupConstant: string -> LookupConstant.Const.t}
	 -> Xml.Program.t
   end



