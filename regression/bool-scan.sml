fun doit s =
   case Bool.fromString s of
      NONE => print "NONE\n"
    | SOME b => print ("SOME " ^ Bool.toString b ^ "\n")

val l = ["true", "false", "TRUE", "FALSE", "TrUeZ", "fAlSeZ",
         " true", "\nfalse", " \tTRUE", "\nFALSE", "\n\nTrUeZ", "\n\nfAlSeZ",
         "tr ue", "f a l s e", "T_R_U_E_", "F1A2L3S4E3"]

val () = List.app doit l
