type int = Int.t
type word = Word.t
   
signature FFI_STRUCTS = 
   sig
      structure CFunction: C_FUNCTION
      structure CType: C_TYPE
      sharing CFunction.CType = CType
   end

signature FFI = 
   sig
      include FFI_STRUCTS

      val addExport: {args: CType.t vector,
		      convention: CFunction.Convention.t,
		      name: string,
		      res: CType.t option} -> int
      val declareExports: {print: string -> unit} -> unit
      val declareHeaders: {print: string -> unit} -> unit
      val numExports: unit -> int
   end
