(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature C_TYPE_STRUCTS = 
   sig
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE
   end

signature C_TYPE = 
   sig
      include C_TYPE_STRUCTS

      datatype t =
         Int8
       | Int16
       | Int32
       | Int64
       | Pointer
       | Real32
       | Real64
       | Word8
       | Word16
       | Word32
       | Word64

      val align: t * Bytes.t -> Bytes.t
      val all: t list
      val bool: t
      val char: t
      val equals: t * t -> bool
      val memo: (t -> 'a) -> t -> 'a
      (* name: I{8,16,32,64} R{32,64} W{8,16,32,64} *)
      val name: t -> string
      val layout: t -> Layout.t
      val pointer: t
      val real: RealSize.t -> t
      val size: t -> Bytes.t
      val thread: t
      val toString: t -> string
      val word: WordSize.t * {signed: bool} -> t
   end
