
signature SOCKET =
  sig
     type ('af, 'sock_type) sock
     val sockToFD: ('af, 'sock_type) sock -> Posix.FileSys.file_desc
     val fdToSock: Posix.FileSys.file_desc -> ('af, 'sock_type) sock
     type 'af sock_addr
     type dgram
     type 'mode stream
     type passive
     type active
     structure AF: 
        sig
	   type addr_family = NetHostDB.addr_family
	   val list: unit -> (string * addr_family) list
	   val toString: addr_family -> string
	   val fromString: string -> addr_family option
	end
     structure SOCK: 
        sig
	   eqtype sock_type
           val stream: sock_type
	   val dgram: sock_type
	   val list: unit -> (string * sock_type) list
	   val toString: sock_type -> string
	   val fromString: string -> sock_type option
	end
     structure Ctl: 
        sig
	   val getDEBUG: ('af, 'sock_type) sock -> bool
	   val setDEBUG: ('af, 'sock_type) sock * bool -> unit
	   val getREUSEADDR: ('af, 'sock_type) sock -> bool
	   val setREUSEADDR: ('af, 'sock_type) sock * bool -> unit
	   val getKEEPALIVE: ('af, 'sock_type) sock -> bool
	   val setKEEPALIVE: ('af, 'sock_type) sock * bool -> unit
	   val getDONTROUTE: ('af, 'sock_type) sock -> bool
	   val setDONTROUTE: ('af, 'sock_type) sock * bool -> unit
	   val getLINGER: ('af, 'sock_type) sock -> Time.time option
	   val setLINGER: ('af, 'sock_type) sock * Time.time option -> unit
	   val getBROADCAST: ('af, 'sock_type) sock -> bool
	   val setBROADCAST: ('af, 'sock_type) sock * bool -> unit
	   val getOOBINLINE: ('af, 'sock_type) sock -> bool
	   val setOOBINLINE: ('af, 'sock_type) sock * bool -> unit
	   val getSNDBUF: ('af, 'sock_type) sock -> int
	   val setSNDBUF: ('af, 'sock_type) sock * int -> unit
	   val getRCVBUF: ('af, 'sock_type) sock -> int
	   val setRCVBUF: ('af, 'sock_type) sock * int -> unit
	   val getTYPE: ('af, 'sock_type) sock -> SOCK.sock_type
	   val getERROR: ('af, 'sock_type) sock -> bool
	   val getPeerName: ('af, 'sock_type) sock -> 'af sock_addr
	   val getSockName: ('af, 'sock_type) sock -> 'af sock_addr
	   val setNBIO: ('af, 'sock_type) sock * bool -> unit
	   val getNREAD: ('af, 'sock_type) sock -> int
	   val getATMARK: ('af, active stream) sock -> bool
	end
     val sameAddr: 'af sock_addr * 'af sock_addr -> bool
     val familyOfAddr: 'af sock_addr -> AF.addr_family
     val accept: ('af, passive stream) sock -> ('af, active stream) sock * 'af sock_addr
     val bind: ('af, 'sock_type) sock * 'af sock_addr -> unit
     val connect: ('af, 'sock_type) sock * 'af sock_addr -> unit
     val listen: ('af, passive stream) sock * int -> unit
     val close: ('af, 'sock_type) sock -> unit
     datatype shutdown_mode = 
        NO_RECVS
      | NO_SENDS
      | NO_RECVS_OR_SENDS
     val shutdown: ('af, 'sock_type stream) sock * shutdown_mode -> unit
     type sock_desc
     val sockDesc : ('af, 'sock_type) sock -> sock_desc
     val sameDesc : sock_desc * sock_desc -> bool
     val select : {rds : sock_desc list,
		   wrs : sock_desc list,
		   exs : sock_desc list,
		   timeout : Time.time option} -> 
	          {rds : sock_desc list,
		   wrs : sock_desc list,
		   exs : sock_desc list}
     val ioDesc : ('af, 'sock_type) sock -> OS.IO.iodesc
     type out_flags = {don't_route : bool, oob : bool}
     type in_flags = {peek : bool, oob : bool}
     type 'a buf = {buf : 'a, i : int, sz : int option}
     val sendVec: ('af, active stream) sock * Word8Vector.vector buf -> 
		  int
     val sendArr: ('af, active stream) sock * Word8Array.array buf -> 
		  int
     val sendVec': ('af, active stream) sock * Word8Vector.vector buf * 
		   out_flags -> 
		   int
     val sendArr': ('af, active stream) sock * Word8Array.array buf * 
		   out_flags -> 
		   int
     val sendVecTo: ('af, dgram) sock * 'af sock_addr * 
		    Word8Vector.vector buf -> 
		    int
     val sendArrTo: ('af, dgram) sock * 'af sock_addr * 
		    Word8Array.array buf -> 
		    int
     val sendVecTo': ('af, dgram) sock * 'af sock_addr * 
		     Word8Vector.vector buf * 
		     out_flags -> 
		     int
     val sendArrTo': ('af, dgram) sock * 'af sock_addr * 
		     Word8Array.array buf * 
		     out_flags -> 
		     int
     val recvVec: ('af, active stream) sock * int -> 
		  Word8Vector.vector
     val recvArr: ('af, active stream) sock * Word8Array.array buf -> 
		  int
     val recvVec': ('af, active stream) sock * int * 
		   in_flags -> 
		   Word8Vector.vector
     val recvArr': ('af, active stream) sock * Word8Array.array buf * 
		   in_flags -> 
		   int
     val recvVecFrom: ('af, dgram) sock * int -> 
                      Word8Vector.vector * 'sock_type sock_addr
     val recvArrFrom: ('af, dgram) sock * Word8Array.array buf -> 
                      int * 'af sock_addr
     val recvVecFrom': ('af, dgram) sock * int * in_flags -> 
                       Word8Vector.vector * 'sock_type sock_addr
     val recvArrFrom': ('af, dgram) sock * Word8Array.array buf * in_flags -> 
                       int * 'af sock_addr
  end

signature SOCKET_EXTRA =
  sig
    include SOCKET
    val sockToWord: ('af, 'sock_type) sock -> SysWord.word
    val wordToSock: SysWord.word -> ('af, 'sock_type) sock
(*
    val sockToFD: ('af, 'sock_type) sock -> Posix.FileSys.file_desc
    val fdToSock: Posix.FileSys.file_desc -> ('af, 'sock_type) sock
*)
    type pre_sock_addr
    val unpackSockAddr: 'af sock_addr -> Word8Vector.vector
    val new_sock_addr: unit -> (pre_sock_addr * int ref * (unit -> 'af sock_addr))

    structure CtlExtra:
       sig
	  type level = int
	  type optname = int
	  type request = int

	  val getSockOptWord : 
	    level * optname -> 
	    ('af, 'sock_type) sock -> word
	  val setSockOptWord :
	    level * optname ->
	    ('af, 'sock_type) sock * word -> unit
	  val getSockOptInt : 
	    level * optname -> 
	    ('af, 'sock_type) sock -> int
	  val setSockOptInt :
	    level * optname ->
	    ('af, 'sock_type) sock * int -> unit
	  val getSockOptBool : 
	    level * optname -> 
	    ('af, 'sock_type) sock -> bool
	  val setSockOptBool :
	    level * optname ->
	    ('af, 'sock_type) sock * bool -> unit

	  val getIOCtlWord : 
	    request -> 
	    ('af, 'sock_type) sock -> word
	  val setIOCtlWord :
	    request ->
	    ('af, 'sock_type) sock * word -> unit
	  val getIOCtlInt : 
	    request -> 
	    ('af, 'sock_type) sock -> int
	  val setIOCtlInt :
	    request ->
	    ('af, 'sock_type) sock * int -> unit
	  val getIOCtlBool : 
	    request -> 
	    ('af, 'sock_type) sock -> bool
	  val setIOCtlBool :
	    request ->
	    ('af, 'sock_type) sock * bool -> unit
       end
  end