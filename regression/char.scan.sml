val dquote = "\""
   
val _ = print (concat [Bool.toString (isSome (Char.fromString dquote)), "\n"])

val scan: string -> unit =
   fn s =>
   let
      val n = String.size s
      fun reader i =
         if i = n
            then NONE
         else SOME (String.sub (s, i), i + 1)
   in
      case Char.scan reader 0 of
         NONE => print "NONE\n"
       | SOME (c, i) => print (concat [str c, " at ", Int.toString i,
                                       " of ", Int.toString n, "\n"])
   end

val _ =
   List.app scan ["a\\ \\", "\\ \\a", "\\ \\a\\ \\", "\\ \\\\ \\a",
                  "\\ \\"]
