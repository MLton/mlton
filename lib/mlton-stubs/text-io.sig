signature MLTON_TEXT_IO =
   sig
(*      val equalsIn: TextIO.instream * TextIO.instream -> bool *)
(*      val equalsOut: TextIO.outstream * TextIO.outstream -> bool *)
      val inFd: TextIO.instream -> Posix.IO.file_desc
      (* mkstemp s creates and opens a new temp file with prefix s, returning
       * the name of the temp file and the outstream to write to it.
       *)
      val mkstemp: string -> string * TextIO.outstream
      (* mkstemps is like mkstemp, except it has both a prefix and suffix. *)
      val mkstemps: {prefix: string, suffix: string} -> string * TextIO.outstream
      val newIn: Posix.IO.file_desc -> TextIO.instream
      val newOut: Posix.IO.file_desc -> TextIO.outstream
      val outFd: TextIO.outstream -> Posix.IO.file_desc
(*      val setIn: TextIO.instream * TextIO.instream -> unit *)
   end
