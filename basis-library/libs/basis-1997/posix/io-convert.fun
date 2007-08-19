(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PosixIOConvert (structure IO: POSIX_IO): POSIX_IO_1997 =
  struct
     open IO

     structure FD =
        struct
           open FD
           structure Flags = FlagsConvert (structure Flags = FD)
           open Flags
        end

     structure O =
        struct
           open O
           structure Flags = FlagsConvert (structure Flags = O)
           open Flags
        end

     fun readArr (fd, {buf, i, sz}) =
        IO.readArr (fd, Word8ArraySlice.slice (buf, i, sz))

     fun writeArr (fd, {buf, i, sz}) =
        IO.writeArr (fd, Word8ArraySlice.slice (buf, i, sz))

     fun writeVec (fd, {buf, i, sz}) =
        IO.writeVec (fd, Word8VectorSlice.slice (buf, i, sz))
  end
