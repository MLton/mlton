(* print out the command name and all of the command line arguments on separate
 lines *)

val _ =
   (print(CommandLine.name()) ;
    print "\n" ;
    app (fn s => (print s ; print "\n")) (CommandLine.arguments()))
