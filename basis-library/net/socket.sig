signature SOCKET =
   sig
      type active
      type dgram
      type in_flags = {peek: bool, oob: bool}
      type out_flags = {don't_route: bool, oob: bool}
      type passive
      datatype shutdown_mode =
         NO_RECVS
       | NO_SENDS
       | NO_RECVS_OR_SENDS
      type ('af,'sock_type) sock
      type 'af sock_addr
      type sock_desc
      type 'mode stream

      structure AF:
         sig
            type addr_family = NetHostDB.addr_family

            val fromString: string -> addr_family option
            val list: unit -> (string * addr_family) list
            val toString: addr_family -> string
         end

      structure SOCK:
         sig
            eqtype sock_type

            val dgram: sock_type
            val fromString: string -> sock_type option
            val list: unit -> (string * sock_type) list
            val stream: sock_type
            val toString: sock_type -> string
         end

      structure Ctl:
         sig
            val getATMARK: ('af, active stream) sock -> bool
            val getBROADCAST: ('af, 'sock_type) sock -> bool
            val getDEBUG: ('af, 'sock_type) sock -> bool
            val getDONTROUTE: ('af, 'sock_type) sock -> bool
            val getERROR: ('af, 'sock_type) sock -> bool
            val getKEEPALIVE: ('af, 'sock_type) sock -> bool
            val getLINGER: ('af, 'sock_type) sock -> Time.time option
            val getNREAD: ('af, 'sock_type) sock -> int
            val getOOBINLINE: ('af, 'sock_type) sock -> bool
            val getPeerName: ('af, 'sock_type) sock -> 'af sock_addr
            val getRCVBUF: ('af, 'sock_type) sock -> int
            val getREUSEADDR: ('af, 'sock_type) sock -> bool
            val getSNDBUF: ('af, 'sock_type) sock -> int
            val getSockName: ('af, 'sock_type) sock -> 'af sock_addr
            val getTYPE: ('af, 'sock_type) sock -> SOCK.sock_type
            val setBROADCAST: ('af, 'sock_type) sock * bool -> unit
            val setDEBUG: ('af, 'sock_type) sock * bool -> unit
            val setDONTROUTE: ('af, 'sock_type) sock * bool -> unit
            val setKEEPALIVE: ('af, 'sock_type) sock * bool -> unit
            val setLINGER: ('af, 'sock_type) sock * Time.time option -> unit
            val setOOBINLINE: ('af, 'sock_type) sock * bool -> unit
            val setRCVBUF: ('af, 'sock_type) sock * int -> unit
            val setREUSEADDR: ('af, 'sock_type) sock * bool -> unit
            val setSNDBUF: ('af, 'sock_type) sock * int -> unit
         end

      val accept: ('af, passive stream) sock -> (('af, active stream) sock
                                                 * 'af sock_addr)
      val acceptNB: ('af, passive stream) sock -> (('af, active stream) sock
                                                   * 'af sock_addr) option
      val bind: ('af, 'sock_type) sock * 'af sock_addr -> unit
      val close: ('af, 'sock_type) sock -> unit
      val connect: ('af, 'sock_type) sock * 'af sock_addr -> unit
      val connectNB: ('af, 'sock_type) sock * 'af sock_addr -> bool
      val familyOfAddr: 'af sock_addr -> AF.addr_family
      val ioDesc: ('af, 'sock_type) sock -> OS.IO.iodesc
      val listen: ('af, passive stream) sock * int -> unit
      val recvArr: ('af, active stream) sock * Word8ArraySlice.slice -> int
      val recvArr': (('af, active stream) sock
                     * Word8ArraySlice.slice
                     * in_flags) -> int
      val recvArrFrom: (('af, dgram) sock * Word8ArraySlice.slice
                        -> int * 'af sock_addr)
      val recvArrFrom': (('af, dgram) sock * Word8ArraySlice.slice * in_flags
                         -> int * 'af sock_addr)
      val recvArrFromNB: (('af, dgram) sock * Word8ArraySlice.slice
                          -> (int * 'af sock_addr) option)
      val recvArrFromNB': (('af, dgram) sock * Word8ArraySlice.slice * in_flags
                           -> (int * 'af sock_addr) option)
      val recvArrNB: (('af, active stream) sock
                      * Word8ArraySlice.slice) -> int option
      val recvArrNB': (('af, active stream) sock
                       * Word8ArraySlice.slice
                       * in_flags) -> int option
      val recvVec: ('af, active stream) sock * int -> Word8Vector.vector
      val recvVec': (('af, active stream) sock * int * in_flags
                     -> Word8Vector.vector)
      val recvVecFrom: (('af, dgram) sock * int
                        -> Word8Vector.vector * 'af sock_addr)
      val recvVecFrom': (('af, dgram) sock * int * in_flags
                         -> Word8Vector.vector * 'af sock_addr)
      val recvVecFromNB: (('af, dgram) sock * int
                          -> (Word8Vector.vector * 'af sock_addr) option)
      val recvVecFromNB': (('af, dgram) sock * int * in_flags
                           -> (Word8Vector.vector * 'af sock_addr) option)
      val recvVecNB: ('af, active stream) sock * int -> Word8Vector.vector option
      val recvVecNB': (('af, active stream) sock * int * in_flags
                       -> Word8Vector.vector option)
      val sameAddr: 'af sock_addr * 'af sock_addr -> bool
      val sameDesc: sock_desc * sock_desc -> bool
      val select: {exs: sock_desc list,
                   rds: sock_desc list,
                   timeout: Time.time option,
                   wrs: sock_desc list} -> {exs: sock_desc list,
                                            rds: sock_desc list,
                                            wrs: sock_desc list}
      val sendArr: ('af, active stream) sock * Word8ArraySlice.slice -> int
      val sendArr': (('af, active stream) sock
                     * Word8ArraySlice.slice
                     * out_flags) -> int
      val sendArrNB: (('af, active stream) sock * Word8ArraySlice.slice
                      -> int option)
      val sendArrNB': (('af, active stream) sock
                       * Word8ArraySlice.slice
                       * out_flags) -> int option
      val sendArrTo: (('af, dgram) sock
                      * 'af sock_addr
                      * Word8ArraySlice.slice) -> unit
      val sendArrTo': (('af, dgram) sock
                       * 'af sock_addr
                       * Word8ArraySlice.slice
                       * out_flags) -> unit
      val sendArrToNB: (('af, dgram) sock
                        * 'af sock_addr
                        * Word8ArraySlice.slice) -> bool
      val sendArrToNB': (('af, dgram) sock
                         * 'af sock_addr
                         * Word8ArraySlice.slice
                         * out_flags) -> bool
      val sendVec: ('af, active stream) sock * Word8VectorSlice.slice -> int
      val sendVec': (('af, active stream) sock
                     * Word8VectorSlice.slice
                     * out_flags) -> int
      val sendVecNB: (('af, active stream) sock
                      * Word8VectorSlice.slice) -> int option
      val sendVecNB': (('af, active stream) sock
                       * Word8VectorSlice.slice
                       * out_flags) -> int option
      val sendVecTo: (('af, dgram) sock
                      * 'af sock_addr
                      * Word8VectorSlice.slice) -> unit
      val sendVecTo': (('af, dgram) sock
                       * 'af sock_addr
                       * Word8VectorSlice.slice
                       * out_flags) -> unit
      val sendVecToNB: (('af, dgram) sock
                        * 'af sock_addr
                        * Word8VectorSlice.slice) -> bool
      val sendVecToNB': (('af, dgram) sock
                         * 'af sock_addr
                         * Word8VectorSlice.slice
                         * out_flags) -> bool
      val shutdown: ('af, 'mode stream) sock * shutdown_mode -> unit
      val sockDesc: ('af, 'sock_type) sock -> sock_desc
   end

signature SOCKET_EXTRA =
  sig
    include SOCKET
    val sockToWord: ('af, 'sock_type) sock -> SysWord.word
    val wordToSock: SysWord.word -> ('af, 'sock_type) sock
    val sockToFD: ('af, 'sock_type) sock -> Posix.FileSys.file_desc
    val fdToSock: Posix.FileSys.file_desc -> ('af, 'sock_type) sock
    type pre_sock_addr
    val unpackSockAddr: 'af sock_addr -> Word8.word vector
    val new_sock_addr: unit -> (pre_sock_addr * C_Socklen.t ref * (unit -> 'af sock_addr))

    structure CtlExtra:
       sig
          type level = C_Int.int
          type optname = C_Int.int
          type request = C_Int.int

          val getERROR: ('af, 'sock_type) sock -> (string * Posix.Error.syserror option) option
          val getSockOptInt: level * optname -> ('af, 'sock_type) sock -> C_Int.int
          val setSockOptInt: level * optname -> ('af, 'sock_type) sock * C_Int.int -> unit
          val getSockOptBool: level * optname -> ('af, 'sock_type) sock -> bool
          val setSockOptBool: level * optname -> ('af, 'sock_type) sock * bool -> unit

          val getIOCtlInt: request -> ('af, 'sock_type) sock -> C_Int.int
          (* val setIOCtlInt: request -> ('af, 'sock_type) sock * C_Int.int -> unit *)
          val getIOCtlBool: request -> ('af, 'sock_type) sock -> bool
          (* val setIOCtlBool: request -> ('af, 'sock_type) sock * bool -> unit *)
       end
  end
