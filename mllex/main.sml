structure Main =
struct

fun usage s =
   Process.usage {usage = "file.lex ...",
		  msg = s}

fun main args =
   let
      val rest =
	 let open Popt
	 in parse {switches = args, opts = []}
	 end
   in
      case rest of
	 Result.No s => usage (concat ["invalid switch: ", s])
       | Result.Yes [] => usage "no files"
       | Result.Yes files => List.foreach (files, LexGen.lexGen)
   end

val main = Process.makeMain main

end
