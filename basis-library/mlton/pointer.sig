(* Copyright (C) 2003-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_POINTER =
   sig
      eqtype t

      val add: t * word -> t
      val compare: t * t -> order
      val diff: t * t -> word
      (* val free: t -> unit *)
      val getInt8: t * int -> Int8.int
      val getInt16: t * int -> Int16.int
      val getInt32: t * int -> Int32.int
      val getInt64: t * int -> Int64.int
      val getPointer: t * int -> t
      val getReal32: t * int -> Real32.real
      val getReal64: t * int -> Real64.real
      val getWord8: t * int -> Word8.word
      val getWord16: t * int -> Word16.word
      val getWord32: t * int -> Word32.word
      val getWord64: t * int -> Word64.word
      val null: t
      val setInt8: t * int * Int8.int -> unit
      val setInt16: t * int * Int16.int -> unit
      val setInt32: t * int * Int32.int -> unit
      val setInt64: t * int * Int64.int -> unit
      val setPointer: t * int * t -> unit
      val setReal32: t * int * Real32.real -> unit
      val setReal64: t * int * Real64.real -> unit
      val setWord8: t * int * Word8.word -> unit
      val setWord16: t * int * Word16.word -> unit
      val setWord32: t * int * Word32.word -> unit
      val setWord64: t * int * Word64.word -> unit
      val sub: t * word -> t
   end
