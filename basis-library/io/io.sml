(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
