type int = Int.t
   
signature C_TYPE_STRUCTS = 
   sig
      structure IntSize: INT_SIZE
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE
   end

signature C_TYPE = 
   sig
      include C_TYPE_STRUCTS
      
      datatype t =
	 Int of IntSize.t
       | Pointer
       | Real of RealSize.t
       | Word of WordSize.t

      val align4: int -> int
      val align8: int -> int
      val align: t * int -> int (* align an address *)	 
      val all: t list
      val bool: t
      val char: t
      val defaultInt: t
      val defaultReal: t
      val defaultWord: t
      val equals: t * t -> bool
      val isPointer: t -> bool
      val memo: (t -> 'a) -> t -> 'a
      (* name: R{32,64} I[8,16,32,64] P W[8,16,32] *)
      val name: t -> string
      val pointer: t
      val layout: t -> Layout.t
      val size: t -> int (* bytes *)
      val toString: t -> string
   end
