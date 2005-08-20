val addr = INetSock.any 0
val socket = INetSock.TCP.socket ()
val _ = Socket.bind (socket, addr)
val _ = Socket.listen (socket, 5)
val addr = Socket.Ctl.getSockName socket

fun read socket : string =
   Byte.unpackStringVec (Word8VectorSlice.full (Socket.recvVec (socket, 100)))

fun readNB socket : string option =
   Option.map (Byte.unpackStringVec o Word8VectorSlice.full)
   (Socket.recvVecNB (socket, 100))
   
fun write (socket, s: string): unit =
   (Socket.sendVec (socket, Word8VectorSlice.full (Byte.stringToBytes s))
    ; ())

val _ =
   print (case Socket.acceptNB socket of
             NONE => "OK\n"
           | SOME _ => "WRONG\n")

val _ =
   case Posix.Process.fork () of
      NONE =>
         let
            val _ = Posix.Process.sleep (Time.fromSeconds 1)
            val (socket, _) = Socket.accept socket
            val _ = print (read socket)
            val _ = print (case readNB socket of
                              NONE => "NONE\n"
                            | SOME s => s)
            val _ = write (socket, "goodbye, world\n");
            val _ = Socket.close socket
         in
            ()
         end
    | SOME pid => 
         let
            val socket' = INetSock.TCP.socket ()
            val _ = Socket.connect (socket', addr)
            val _ = write (socket', "hello, world\n")
            val _ = print (read socket')
            val _ = Socket.close socket'
            val (pid', status)  = Posix.Process.wait ()
         in
            if pid = pid' andalso status = Posix.Process.W_EXITED
               then ()
            else print "child failed\n"
         end
