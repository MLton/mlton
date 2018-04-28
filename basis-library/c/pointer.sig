(* Copyright (C) 2010 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature C_POINTER =
sig
   type t = MLton.Pointer.t
   val add: t * C_Ptrdiff.t -> t
   val compare: t * t -> order
   val diff: t * t -> C_Ptrdiff.t
   val fromWord: C_Size.t -> t
   val getC_SChar: t * C_Ptrdiff.t -> C_SChar.t
   val getC_UChar: t * C_Ptrdiff.t -> C_UChar.t
   val getC_SShort: t * C_Ptrdiff.t -> C_SShort.t
   val getC_UShort: t * C_Ptrdiff.t -> C_UShort.t
   val getC_SInt: t * C_Ptrdiff.t -> C_SInt.t
   val getC_UInt: t * C_Ptrdiff.t -> C_UInt.t
   val getC_SLong: t * C_Ptrdiff.t -> C_SLong.t
   val getC_ULong: t * C_Ptrdiff.t -> C_ULong.t
   val getC_SLongLong: t * C_Ptrdiff.t -> C_SLongLong.t
   val getC_ULongLong: t * C_Ptrdiff.t -> C_ULongLong.t
   val getC_Float: t * C_Ptrdiff.t -> C_Float.t
   val getC_Double: t * C_Ptrdiff.t -> C_Double.t
   val getC_Size: t * C_Ptrdiff.t -> C_Size.t
   val getC_Ptrdiff: t * C_Ptrdiff.t -> C_Ptrdiff.t
   val getC_Intmax: t * C_Ptrdiff.t -> C_Intmax.t
   val getC_UIntmax: t * C_Ptrdiff.t -> C_UIntmax.t
   val getC_Intptr: t * C_Ptrdiff.t -> C_Intptr.t
   val getC_UIntptr: t * C_Ptrdiff.t -> C_UIntptr.t
   val getC_Pointer: t * C_Ptrdiff.t -> t
   val getInt8: t * C_Ptrdiff.t -> Int8.int
   val getInt16: t * C_Ptrdiff.t -> Int16.int
   val getInt32: t * C_Ptrdiff.t -> Int32.int
   val getInt64: t * C_Ptrdiff.t -> Int64.int
   val getReal32: t * C_Ptrdiff.t -> Real32.real
   val getReal64: t * C_Ptrdiff.t -> Real64.real
   val getWord8: t * C_Ptrdiff.t -> Word8.word
   val getWord16: t * C_Ptrdiff.t -> Word16.word
   val getWord32: t * C_Ptrdiff.t -> Word32.word
   val getWord64: t * C_Ptrdiff.t -> Word64.word
   val isNull: t -> bool
   val null: t
   val setC_SChar: t * C_Ptrdiff.t * C_SChar.t -> unit
   val setC_UChar: t * C_Ptrdiff.t * C_UChar.t -> unit
   val setC_SShort: t * C_Ptrdiff.t * C_SShort.t -> unit
   val setC_UShort: t * C_Ptrdiff.t * C_UShort.t -> unit
   val setC_SInt: t * C_Ptrdiff.t * C_SInt.t -> unit
   val setC_UInt: t * C_Ptrdiff.t * C_UInt.t -> unit
   val setC_SLong: t * C_Ptrdiff.t * C_SLong.t -> unit
   val setC_ULong: t * C_Ptrdiff.t * C_ULong.t -> unit
   val setC_SLongLong: t * C_Ptrdiff.t * C_SLongLong.t -> unit
   val setC_ULongLong: t * C_Ptrdiff.t * C_ULongLong.t  -> unit
   val setC_Float: t * C_Ptrdiff.t * C_Float.t -> unit
   val setC_Double: t * C_Ptrdiff.t * C_Double.t -> unit
   val setC_Size: t * C_Ptrdiff.t * C_Size.t -> unit
   val setC_Ptrdiff: t * C_Ptrdiff.t * C_Ptrdiff.t -> unit
   val setC_Intmax: t * C_Ptrdiff.t * C_Intmax.t -> unit
   val setC_UIntmax: t * C_Ptrdiff.t * C_UIntmax.t -> unit
   val setC_Intptr: t * C_Ptrdiff.t * C_Intptr.t -> unit
   val setC_UIntptr: t * C_Ptrdiff.t * C_UIntptr.t -> unit
   val setC_Pointer: t * C_Ptrdiff.t * t -> unit
   val setInt8: t * C_Ptrdiff.t * Int8.int -> unit
   val setInt16: t * C_Ptrdiff.t * Int16.int -> unit
   val setInt32: t * C_Ptrdiff.t * Int32.int -> unit
   val setInt64: t * C_Ptrdiff.t * Int64.int -> unit
   val setReal32: t * C_Ptrdiff.t * Real32.real -> unit
   val setReal64: t * C_Ptrdiff.t * Real64.real -> unit
   val setWord8: t * C_Ptrdiff.t * Word8.word -> unit
   val setWord16: t * C_Ptrdiff.t * Word16.word -> unit
   val setWord32: t * C_Ptrdiff.t * Word32.word -> unit
   val setWord64: t * C_Ptrdiff.t * Word64.word -> unit
   val sizeofPointer: C_Size.t
   val sub: t * C_Ptrdiff.t -> t
   val toWord: t -> C_Size.t
end