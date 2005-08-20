signature IO_1997 =
   sig
      exception Io of {cause: exn,
                       function: string,
                       name: string}
      exception BlockingNotSupported
      exception NonblockingNotSupported
      exception RandomAccessNotSupported
      exception TerminatedStream
      exception ClosedStream
      datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
   end
