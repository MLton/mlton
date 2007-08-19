(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure IO: IO =
   struct
      exception BlockingNotSupported

      exception ClosedStream

      exception Io of {cause : exn,
                       function : string,
                       name : string}

      val _ =
         General.addExnMessager
         (fn e =>
          case e of
             Io {cause, function, name, ...} => 
                SOME (concat ["Io: ", function, " \"", name, "\" failed with ",
                              exnMessage cause])
           | _ => NONE)

      exception NonblockingNotSupported

      exception RandomAccessNotSupported

      datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
   end
