structure Main =
struct

fun usage s =
   Process.usage {usage = "file.grm",
		  msg = s}

fun main args =
   let
      val rest =
	 let open Popt
	 in parse {switches = args,
		   opts = []}
	 end
   in case rest of
      Result.No s => usage (concat ["invalid switch: ", s])
    | Result.Yes [file] => ParseGen.parseGen file
    | _ => usage "too many files"
   end

val main = Process.makeMain main

end
