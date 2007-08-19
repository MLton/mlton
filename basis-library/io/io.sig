(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature IO =
   sig
      exception Io of {name : string,
                       function : string,
                       cause : exn}
      exception BlockingNotSupported
      exception NonblockingNotSupported
      exception RandomAccessNotSupported
      exception ClosedStream
      datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
   end
