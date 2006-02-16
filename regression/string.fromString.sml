fun check (s, s') =
   case String.fromString s of
      NONE => print "WRONG  NONE\n"
    | SOME s'' =>
         if s' = s''
            then print (concat ["OK  [", s', "]\n"])
         else print (concat ["WRONG  [", s', "] [", s'', "]\n"])

val _ =
   List.app check
   [("abc\"def", "abc"),
     ("\\q", ""),
     ("a\^D", "a"),
     ("a\\ \\\\q", "a"),
     ("\\ \\", ""),
     ("", ""),
     ("\\ \\\^D", ""),
     ("\\ a", "")]
