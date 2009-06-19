(* Notice: This test will not be passed on platforms like Win32!
           (due to behind-the-scenes CR/LF <=> LF conversions). *)

val filename = OS.FileSys.tmpName ()

fun testRange (start, length) =
      let
        val allChars = CharVector.tabulate(length, fn i => chr ((i + start) mod 256))

        val outStr = TextIO.openOut filename
        val _ = TextIO.output (outStr, allChars)
        val _ = TextIO.closeOut outStr

        val inStr = TextIO.openIn filename
        val readChars = TextIO.inputAll inStr
        val _ = TextIO.closeIn inStr

        val _ = OS.FileSys.remove filename

        fun testCharF (c, cnt) =
              let
                val readC = CharVector.sub(readChars, cnt)
                val _ = if c = readC then
                          ()
                        else
                          print ("Error at index: " ^ (Int.toString cnt) ^ ": " ^
                                 (Char.toString c) ^ " <> " ^ (Char.toString readC) ^ "\n")
              in
                cnt + 1
              end

        val _ = CharVector.foldl testCharF 0 allChars
      in
        ()
      end

val _ = testRange (0, 256)
val _ = print "basic test of writing and reading back all characters done\n"
val _ = List.tabulate(256, fn i => List.tabulate(257, fn i2 => testRange (i, i2)))
val _ = print "test of writing files of all possible characters in strings of lengths 0-256 finished\n"
val _ = List.tabulate(6, fn i => List.tabulate(5000, fn i2 => testRange (i, i2)))

val _ = print "test finished\n"
