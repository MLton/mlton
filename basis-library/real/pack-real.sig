signature PACK_REAL =
   sig
      type real

      val bytesPerElem: int 
      val fromBytes: Word8Vector.vector -> real 
      val isBigEndian: bool 
      val subArr: Word8Array.array * int -> real 
      val subVec: Word8Vector.vector * int -> real 
      val toBytes: real -> Word8Vector.vector 
      val update: Word8Array.array * int * real -> unit
   end
