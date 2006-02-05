signature PACK_WORD =
   sig
      val bytesPerElem: int 
      val isBigEndian: bool 
      val subArr: Word8Array.array * int -> LargeWord.word 
      val subArrX: Word8Array.array * int -> LargeWord.word 
      val subVec: Word8Vector.vector * int -> LargeWord.word 
      val subVecX: Word8Vector.vector * int -> LargeWord.word 
      val update: Word8Array.array * int * LargeWord.word -> unit
   end
