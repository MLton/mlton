fun 'a printSize (name: string, value: 'a): unit=
   (print "The size of "
    ; print name
    ; print " is "
    ; print (Int.toString (MLton.size value))
    ; print " bytes.\n")

val l = [1, 2, 3, 4]

val _ =
   (
    printSize ("a char", #"c")
    ; printSize ("an int list of length 4", l)
    ; printSize ("a string of length 10", "0123456789")
    ; printSize ("an int array of length 10", Array.tabulate (10, fn _ => 0))
    ; printSize ("a double array of length 10", Array.tabulate (10, fn _ => 0.0))
    ; printSize ("a useless function", fn _ => 13)
    )

(* This is here so that the list is "useful".
 * If it were removed, then the optimizer (remove-unused-constructors)
 * would remove l entirely.
 *)
val _ = if 10 = foldl (op +) 0 l
	   then ()
	else raise Fail "bug"
   
				  
