type int = Int.t
   
signature C_TYPE_STRUCTS = 
   sig
   end

signature C_TYPE = 
   sig
      include C_TYPE_STRUCTS

      datatype t =
	 Pointer
       | Real32
       | Real64
       | Word8
       | Word16
       | Word32
       | Word64

      val align: t * Bytes.t -> Bytes.t
      val all: t list
      val equals: t * t -> bool
      val memo: (t -> 'a) -> t -> 'a
      (* name: R{32,64} W{8,16,32,64} *)
      val name: t -> string
      val layout: t -> Layout.t
      val size: t -> Bytes.t
      val toString: t -> string
   end
