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

signature PACK_REAL_EXTRA =
   sig
      include PACK_REAL
      val unsafeSubVec: Word8Vector.vector * int -> real 
      val unsafeSubArr: Word8Array.array * int -> real 
      val unsafeUpdate: Word8Array.array * int * real -> unit
   end
