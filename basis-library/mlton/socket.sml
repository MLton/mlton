structure Socket: MLTON_SOCKET =
struct

structure Prim = Primitive.Socket
open Prim

structure Port =
   struct
      type t = port
   end

structure Address =
   struct
      type t = word
   end

structure Host =
   struct
      structure Prim = Prim.Host
	 
      type t = {name: string}

      fun get (b: bool): t option =
	 if b
	    then SOME {name = C.CS.toString (Prim.name ())}
	 else NONE

      val getByAddress = get o Prim.getByAddress
      val getByName = get o Prim.getByName o String.nullTerm
   end

type t = socket
   
val listen: unit -> port * socket =
   fn () =>
   let
      val port = ref 0
      val socket = ref 0
      val _ = Posix.Error.checkResult (Prim.listen (port, socket))
   in (!port, !socket)
   end

val listenAt: port -> socket =
   fn port =>
   let
      val socket = ref 0
      val _ = Posix.Error.checkResult (Prim.listen (ref port, socket))
   in !socket
   end

fun fdToIO fd =
   let
      val _ = Posix.Error.checkResult fd
      val fd = Posix.FileSys.wordToFD (SysWord.fromInt fd)
      val ins = TextIO.newIn fd
      val out = TextIO.newOut (Posix.IO.dup fd)
   in (ins, out)
   end

fun accept s =
   let val (ins, out) = fdToIO (Prim.accept s)
   in (Prim.Addr.address (),
       Prim.Addr.port (),
       ins,
       out)
   end

fun connect (host, port) =
   fdToIO (Prim.connect (String.nullTerm host, port))

fun shutdown (PosixPrimitive.FD n, how: int): unit =
   PosixError.checkResult (Prim.shutdown (n, how))

fun shutdownRead ins =
   shutdown (TextIO.inFd ins, Prim.shutdownRead)

fun shutdownWrite out =
   (TextIO.flushOut out
    ; shutdown (TextIO.outFd out, Prim.shutdownWrite))

end
