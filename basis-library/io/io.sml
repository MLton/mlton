(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure IO: IO =
   struct
      exception Io of {name: string, function: string, cause: exn} 
      exception BlockingNotSupported
      exception NonblockingNotSupported
      exception RandomAccessNotSupported
      exception TerminatedStream
      exception ClosedStream
      datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
   end
