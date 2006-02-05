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
