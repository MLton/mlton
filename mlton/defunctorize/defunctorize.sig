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
