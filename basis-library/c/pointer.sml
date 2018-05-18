(* Copyright (C) 2010 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure C_Pointer : C_POINTER =
struct

open Primitive.MLton.Pointer

val sizeofPointer = C_Size.div (C_Size.fromInt C_Size.wordSize, 0w8)

local
   structure S =
      C_SChar_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fInt8 = getInt8
       val fInt16 = getInt16
       val fInt32 = getInt32
       val fInt64 = getInt64)
in
   val getC_SChar = S.f
end
local
   structure S =
      C_UChar_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fWord8 = getWord8
       val fWord16 = getWord16
       val fWord32 = getWord32
       val fWord64 = getWord64)
in
   val getC_UChar = S.f
end

local
   structure S =
      C_SShort_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fInt8 = getInt8
       val fInt16 = getInt16
       val fInt32 = getInt32
       val fInt64 = getInt64)
in
   val getC_SShort = S.f
end
local
   structure S =
      C_UShort_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fWord8 = getWord8
       val fWord16 = getWord16
       val fWord32 = getWord32
       val fWord64 = getWord64)
in
   val getC_UShort = S.f
end

local
   structure S =
      C_SInt_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fInt8 = getInt8
       val fInt16 = getInt16
       val fInt32 = getInt32
       val fInt64 = getInt64)
in
   val getC_SInt = S.f
end
local
   structure S =
      C_UInt_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fWord8 = getWord8
       val fWord16 = getWord16
       val fWord32 = getWord32
       val fWord64 = getWord64)
in
   val getC_UInt = S.f
end

local
   structure S =
      C_SLong_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fInt8 = getInt8
       val fInt16 = getInt16
       val fInt32 = getInt32
       val fInt64 = getInt64)
in
   val getC_SLong = S.f
end
local
   structure S =
      C_ULong_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fWord8 = getWord8
       val fWord16 = getWord16
       val fWord32 = getWord32
       val fWord64 = getWord64)
in
   val getC_ULong = S.f
end

local
   structure S =
      C_SLongLong_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fInt8 = getInt8
       val fInt16 = getInt16
       val fInt32 = getInt32
       val fInt64 = getInt64)
in
   val getC_SLongLong = S.f
end
local
   structure S =
      C_ULongLong_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fWord8 = getWord8
       val fWord16 = getWord16
       val fWord32 = getWord32
       val fWord64 = getWord64)
in
   val getC_ULongLong = S.f
end

local
   structure S =
      C_Float_ChooseRealN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fReal32 = getReal32
       val fReal64 = getReal64)
in
   val getC_Float = S.f
end
local
   structure S =
      C_Double_ChooseRealN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fReal32 = getReal32
       val fReal64 = getReal64)
in
   val getC_Double = S.f
end

local
   structure S =
      C_Size_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fWord8 = getWord8
       val fWord16 = getWord16
       val fWord32 = getWord32
       val fWord64 = getWord64)
in
   val getC_Size = S.f
end
local
   structure S =
      C_Ptrdiff_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fInt8 = getInt8
       val fInt16 = getInt16
       val fInt32 = getInt32
       val fInt64 = getInt64)
in
   val getC_Ptrdiff = S.f
end

local
   structure S =
      C_Intmax_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fInt8 = getInt8
       val fInt16 = getInt16
       val fInt32 = getInt32
       val fInt64 = getInt64)
in
   val getC_Intmax = S.f
end
local
   structure S =
      C_UIntmax_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fWord8 = getWord8
       val fWord16 = getWord16
       val fWord32 = getWord32
       val fWord64 = getWord64)
in
   val getC_UIntmax = S.f
end

local
   structure S =
      C_Intptr_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fInt8 = getInt8
       val fInt16 = getInt16
       val fInt32 = getInt32
       val fInt64 = getInt64)
in
   val getC_Intptr = S.f
end
local
   structure S =
      C_UIntptr_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t -> 'a
       val fWord8 = getWord8
       val fWord16 = getWord16
       val fWord32 = getWord32
       val fWord64 = getWord64)
in
   val getC_UIntptr = S.f
end

val getC_Pointer = getCPointer


local
   structure S =
      C_SChar_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fInt8 = setInt8
       val fInt16 = setInt16
       val fInt32 = setInt32
       val fInt64 = setInt64)
in
   val setC_SChar = S.f
end
local
   structure S =
      C_UChar_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fWord8 = setWord8
       val fWord16 = setWord16
       val fWord32 = setWord32
       val fWord64 = setWord64)
in
   val setC_UChar = S.f
end

local
   structure S =
      C_SShort_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fInt8 = setInt8
       val fInt16 = setInt16
       val fInt32 = setInt32
       val fInt64 = setInt64)
in
   val setC_SShort = S.f
end
local
   structure S =
      C_UShort_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fWord8 = setWord8
       val fWord16 = setWord16
       val fWord32 = setWord32
       val fWord64 = setWord64)
in
   val setC_UShort = S.f
end

local
   structure S =
      C_SInt_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fInt8 = setInt8
       val fInt16 = setInt16
       val fInt32 = setInt32
       val fInt64 = setInt64)
in
   val setC_SInt = S.f
end
local
   structure S =
      C_UInt_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fWord8 = setWord8
       val fWord16 = setWord16
       val fWord32 = setWord32
       val fWord64 = setWord64)
in
   val setC_UInt = S.f
end

local
   structure S =
      C_SLong_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fInt8 = setInt8
       val fInt16 = setInt16
       val fInt32 = setInt32
       val fInt64 = setInt64)
in
   val setC_SLong = S.f
end
local
   structure S =
      C_ULong_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fWord8 = setWord8
       val fWord16 = setWord16
       val fWord32 = setWord32
       val fWord64 = setWord64)
in
   val setC_ULong = S.f
end

local
   structure S =
      C_SLongLong_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fInt8 = setInt8
       val fInt16 = setInt16
       val fInt32 = setInt32
       val fInt64 = setInt64)
in
   val setC_SLongLong = S.f
end
local
   structure S =
      C_ULongLong_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fWord8 = setWord8
       val fWord16 = setWord16
       val fWord32 = setWord32
       val fWord64 = setWord64)
in
   val setC_ULongLong = S.f
end

local
   structure S =
      C_Float_ChooseRealN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fReal32 = setReal32
       val fReal64 = setReal64)
in
   val setC_Float = S.f
end
local
   structure S =
      C_Double_ChooseRealN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fReal32 = setReal32
       val fReal64 = setReal64)
in
   val setC_Double = S.f
end

local
   structure S =
      C_Size_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fWord8 = setWord8
       val fWord16 = setWord16
       val fWord32 = setWord32
       val fWord64 = setWord64)
in
   val setC_Size = S.f
end
local
   structure S =
      C_Ptrdiff_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fInt8 = setInt8
       val fInt16 = setInt16
       val fInt32 = setInt32
       val fInt64 = setInt64)
in
   val setC_Ptrdiff = S.f
end

local
   structure S =
      C_Intmax_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fInt8 = setInt8
       val fInt16 = setInt16
       val fInt32 = setInt32
       val fInt64 = setInt64)
in
   val setC_Intmax = S.f
end
local
   structure S =
      C_UIntmax_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fWord8 = setWord8
       val fWord16 = setWord16
       val fWord32 = setWord32
       val fWord64 = setWord64)
in
   val setC_UIntmax = S.f
end

local
   structure S =
      C_Intptr_ChooseIntN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fInt8 = setInt8
       val fInt16 = setInt16
       val fInt32 = setInt32
       val fInt64 = setInt64)
in
   val setC_Intptr = S.f
end
local
   structure S =
      C_UIntptr_ChooseWordN
      (type 'a t = t * C_Ptrdiff.t * 'a -> unit
       val fWord8 = setWord8
       val fWord16 = setWord16
       val fWord32 = setWord32
       val fWord64 = setWord64)
in
   val setC_UIntptr = S.f
end

val setC_Pointer = setCPointer


end
