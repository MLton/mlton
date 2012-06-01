fun 'a printSize (name: string, value: 'a): unit=
   (print "The size of "
    ; print name
    ; print " is "
    ; print (Int.toString (MLton.size value))
    ; print " bytes.\n")

val l = [1, 2, 3, 4]

val _ =
   (
    printSize ("an int list of length 4", l)
    ; printSize ("a string of length 10", "0123456789")
    ; printSize ("an int array of length 10", Array.tabulate (10, fn _ => 0))
    ; printSize ("a double array of length 10",
                 Array.tabulate (10, fn _ => 0.0))
    ; printSize ("an array of length 10 of 2-ples of ints",
                 Array.tabulate (10, fn i => (i, i + 1)))
    ; printSize ("a useless function", fn _ => 13)
    )

(* This is here so that the list is "useful".
 * If it were removed, then the optimizer (remove-unused-constructors)
 * would remove l entirely.
 *)
val _ = if 10 = foldl (op +) 0 l
           then ()
        else raise Fail "bug"

local
   open MLton.Cont
in
   val rc: int option t option ref = ref NONE
   val _ =
      case callcc (fn k: int option t => (rc := SOME k; throw (k, NONE))) of
         NONE => ()
       | SOME i => print (concat [Int.toString i, "\n"])
end

val _ = printSize ("a continuation option ref", rc)

val _ =
   case !rc of
      NONE => ()
    | SOME k => (rc := NONE; MLton.Cont.throw (k, SOME 13))
