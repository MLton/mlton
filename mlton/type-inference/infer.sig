(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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

      structure BuildConst:
	 sig
	    datatype t =
	       Bool of bool
	     | Int of int
	 end
	 
      val infer:
	 {program: CoreML.Program.t,
	  lookupBuildConstant: string -> BuildConst.t,
	  lookupConstant: string -> CoreML.Const.t}
	 -> Xml.Program.t
   end



