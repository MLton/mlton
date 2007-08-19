(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PRIM_IO = 
   sig
      type elem
      type vector
      type vector_slice
      type array
      type array_slice

      eqtype pos
      val compare: pos * pos -> order

      datatype reader =
         RD of {avail: unit -> int option,
                block: (unit -> unit) option,
                canInput: (unit -> bool) option,
                chunkSize: int,
                close: unit -> unit,
                endPos: (unit -> pos) option,
                getPos: (unit -> pos) option,
                ioDesc: OS.IO.iodesc option,
                name: string,
                readArr: (array_slice -> int) option,
                readArrNB: (array_slice -> int option) option,
                readVec: (int -> vector) option,
                readVecNB: (int -> vector option) option,
                setPos: (pos -> unit) option,
                verifyPos: (unit -> pos) option}

      datatype writer =
         WR of {block: (unit -> unit) option,
                canOutput: (unit -> bool) option,
                chunkSize: int,
                close: unit -> unit,
                endPos: (unit -> pos) option,
                getPos: (unit -> pos) option,
                ioDesc: OS.IO.iodesc option,
                name: string,
                setPos: (pos -> unit) option,
                verifyPos: (unit -> pos) option,
                writeArr: (array_slice -> int) option,
                writeArrNB: (array_slice -> int option) option,
                writeVec: (vector_slice -> int) option,
                writeVecNB: (vector_slice -> int option) option}

      val openVector: vector -> reader
      val nullRd: unit -> reader
      val nullWr: unit -> writer

      val augmentReader: reader -> reader
      val augmentWriter: writer -> writer 
   end
