(* Copyright (C) 2003-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_FFI =
   sig
      val atomicBegin: unit -> unit
      val atomicEnd: unit -> unit
      val getBool: int -> bool
      val getChar8: int -> Char.char
(*
      val getChar16: int -> Char16.char
      val getChar32: int -> Char32.char
*)
      val getCPointer: int -> MLtonPointer.t
      val getInt8: int -> Int8.int
      val getInt16: int -> Int16.int
      val getInt32: int -> Int32.int
      val getInt64: int -> Int64.int
      val getObjptr: int -> 'a
      val getReal32: int -> Real32.real
      val getReal64: int -> Real64.real
      val getWord8: int -> Word8.word
      val getWord16: int -> Word16.word
      val getWord32: int -> Word32.word
      val getWord64: int -> Word64.word
      val register: int * (unit -> unit) -> unit
      val setBool: bool -> unit
      val setChar8: Char.char -> unit
(*
      val setChar16: Char16.char -> unit
      val setChar32: Char32.char -> unit
*)
      val setCPointer: MLtonPointer.t -> unit
      val setInt8: Int8.int -> unit
      val setInt16: Int16.int -> unit
      val setInt32: Int32.int -> unit
      val setInt64: Int64.int -> unit
      val setObjptr: 'a -> unit
      val setReal32: Real32.real -> unit
      val setReal64: Real64.real -> unit
      val setWord8: Word8.word -> unit
      val setWord16: Word16.word -> unit
      val setWord32: Word32.word -> unit
      val setWord64: Word64.word -> unit
   end
