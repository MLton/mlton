(* Copyright (C) 2003-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_FFI =
   sig
      val getBool: MLtonPointer.t * int -> bool
      val getChar8: MLtonPointer.t * int -> Char.char
(*
      val getChar16: MLtonPointer.t * int -> Char16.char
      val getChar32: MLtonPointer.t * int -> Char32.char
*)
      val getCPointer: MLtonPointer.t * int -> MLtonPointer.t
      val getInt8: MLtonPointer.t * int -> Int8.int
      val getInt16: MLtonPointer.t * int -> Int16.int
      val getInt32: MLtonPointer.t * int -> Int32.int
      val getInt64: MLtonPointer.t * int -> Int64.int
      val getObjptr: MLtonPointer.t * int -> 'a
      val getReal32: MLtonPointer.t * int -> Real32.real
      val getReal64: MLtonPointer.t * int -> Real64.real
      val getWord8: MLtonPointer.t * int -> Word8.word
      val getWord16: MLtonPointer.t * int -> Word16.word
      val getWord32: MLtonPointer.t * int -> Word32.word
      val getWord64: MLtonPointer.t * int -> Word64.word
      val register: int * (MLtonPointer.t -> unit) -> unit
      val setBool: MLtonPointer.t * int * bool -> unit
      val setChar8: MLtonPointer.t * int * Char.char -> unit
(*
      val setChar16: MLtonPointer.t * Char16.char -> unit
      val setChar32: MLtonPointer.t * Char32.char -> unit
*)
      val setCPointer: MLtonPointer.t * int * MLtonPointer.t -> unit
      val setInt8: MLtonPointer.t * int * Int8.int -> unit
      val setInt16: MLtonPointer.t * int * Int16.int -> unit
      val setInt32: MLtonPointer.t * int * Int32.int -> unit
      val setInt64: MLtonPointer.t * int * Int64.int -> unit
      val setObjptr: MLtonPointer.t * int * 'a -> unit
      val setReal32: MLtonPointer.t * int * Real32.real -> unit
      val setReal64: MLtonPointer.t * int * Real64.real -> unit
      val setWord8: MLtonPointer.t * int * Word8.word -> unit
      val setWord16: MLtonPointer.t * int * Word16.word -> unit
      val setWord32: MLtonPointer.t * int * Word32.word -> unit
      val setWord64: MLtonPointer.t * int * Word64.word -> unit
   end
