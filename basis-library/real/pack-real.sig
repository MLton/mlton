signature PACK_REAL =
   sig
      type real

      val bytesPerElem: int 
      val isBigEndian: bool 
      val toBytes: real -> Word8Vector.vector 
      val fromBytes: Word8Vector.vector -> real 
      val subVec: Word8Vector.vector * int -> real 
      val subArr: Word8Array.array * int -> real 
      val update: Word8Array.array * int * real -> unit
   end
