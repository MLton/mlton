type int = Int.t
type word = Word.t
   
signature FFI_STRUCTS = 
   sig
      structure IntSize: INT_SIZE
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE
   end

signature FFI = 
   sig
      include FFI_STRUCTS

      structure Type:
	 sig
	    datatype t =
	       Bool
	     | Char
	     | Int of IntSize.t
	     | Pointer
	     | Real of RealSize.t
	     | Word of WordSize.t

	    val memo: (t -> 'a) -> t -> 'a
	    val toString: t -> string
	 end

      val addExport: {args: Type.t vector,
		      name: string,
		      res: Type.t} -> int
      val declareExports: {print: string -> unit} -> unit
      (* declareHeaders should be called after declareExports. *)
      val declareHeaders: {print: string -> unit} -> unit
      val numExports: unit -> int
   end
