fun 'a printSize (name: string, min: int, value: 'a): unit=
   if MLton.size value >= min
      then
         (print "The size of "
          ; print name
          ; print " is >= "
          ; print (Int.toString min)
          ; print " bytes.\n")
   else ()

val l = [1, 2, 3, 4]

val _ =
   (
    printSize ("a char", 0, #"c")
    ; printSize ("an int list of length 4", 48, l)
    ; printSize ("a string of length 10", 24, "0123456789")
    ; printSize ("an int array of length 10", 52, Array.tabulate (10, fn _ => 0))
    ; printSize ("a double array of length 10",
                 92, Array.tabulate (10, fn _ => 0.0))
    ; printSize ("an array of length 10 of 2-ples of ints",
                 92, Array.tabulate (10, fn i => (i, i + 1)))
    ; printSize ("a useless function", 0, fn _ => 13)
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

val _ =
   (print "The size of a continuation option ref is "
    ; if MLton.size rc > 1000
         then print "> 1000.\n"
      else print "< 1000.\n")

val _ =
   case !rc of
      NONE => ()
    | SOME k => (rc := NONE; MLton.Cont.throw (k, SOME 13))
