val tmp = "tmp"
   
val () =
   let
      open TextIO
      val out = openOut tmp
      val () = output (out, "hello, there\n")
      val () = closeOut out
   in
      ()
   end
      
val () =
   let
      open Posix
      open FileSys
      val fd = openf (tmp, O_WRONLY, O.flags [])
      val () = ftruncate (fd, 5)
      val () = IO.close fd
   in
      ()
   end

val () =
   let
      open TextIO
      val ins = openIn tmp
      val () = print (TextIO.inputAll ins)
      val () = print "\n"
      val () = closeIn ins
   in
      ()
   end

val () = OS.FileSys.remove tmp
