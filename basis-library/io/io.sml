(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure IO: IO =
   struct
      exception BlockingNotSupported
      exception ClosedStream
      exception Io of {cause : exn,
		       function : string,
		       name : string}
      exception NonblockingNotSupported
      exception RandomAccessNotSupported

      datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
   end
