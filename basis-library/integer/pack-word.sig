signature PACK_WORD =
   sig
      val bytesPerElem: int 
      val isBigEndian: bool 
      val subVec: Word8Vector.vector * int -> LargeWord.word 
      val subVecX: Word8Vector.vector * int -> LargeWord.word 
      val subArr: Word8Array.array * int -> LargeWord.word 
      val subArrX: Word8Array.array * int -> LargeWord.word 
      val update: Word8Array.array * int * LargeWord.word -> unit
   end


signature PACK_WORD_EXTRA =
   sig
      include PACK_WORD

      val start: int * int -> int
   end
