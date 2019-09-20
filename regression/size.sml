fun 'a printSize (name: string, min_max: (int * int) option,
                  value: 'a, use: 'a -> unit): unit =
   (print "The size of "
    ; print name
    ; print " is"
    ; let
         val size = MLton.size value
      in
         case min_max of
            NONE => (print " = "
                     ; print (Int.toString size)
                     ; print " bytes.\n")
          | SOME (min, max) =>
               if min <= size andalso size <= max
                  then (print " >= "
                        ; print (Int.toString min)
                        ; print " bytes and <= "
                        ; print (Int.toString max)
                        ; print " bytes.\n")
                  else (print " = "
                        ; print (Int.toString size)
                        ; print " bytes.\n")
      end
    ; use value)

fun chk (x, y) =
   if x = y
      then ()
      else raise Fail "bug"

val l = [1, 2, 3, 4]

val _ =
   (printSize ("a char", NONE, #"c", fn _ => ())
    ; printSize ("an int list of length 4", NONE,
                 List.tabulate (4, fn i => i + 1), fn l =>
                 chk (foldl (op +) 0 l, 10))
    ; printSize ("a string of length 10", NONE,
                 CharVector.tabulate (10, fn i => chr (ord #"0" + i)), fn s =>
                 chk (CharVector.foldl (fn (c,s) => ord c + s) 0 s,  525))
    ; printSize ("an int array of length 10", NONE,
                 Array.tabulate (10, fn i => i), fn a =>
                 chk (Array.foldl (op +) 0 a, 45))
    ; printSize ("a double array of length 10", NONE,
                 Array.tabulate (10, real), fn a =>
                 chk (Real.floor (Array.foldl (op +) 0.0 a), 45))
    ; printSize ("an array of length 10 of 2-ples of ints", NONE,
                 Array.tabulate (10, fn i => (i, i + 1)), fn a =>
                 chk (Array.foldl (fn ((a,b),s) => a + b + s) 0 a, 100))
    ; printSize ("a useless function", NONE,
                 fn _ => 13, fn f => ())
    )
   
local
   open MLton.Cont
in
   val rc: int option t option ref = ref NONE
   val _ =
      case callcc (fn k: int option t => (rc := SOME k; throw (k, NONE))) of
         NONE => ()
       | SOME i => print (concat [Int.toString i, "\n"])
end

val _ = printSize ("a continuation option ref",
                   case !rc of
                      NONE => NONE
                    | SOME _ => SOME (4000, 6000),
                   rc, fn rc =>
                   case !rc of
                      NONE => ()
                    | SOME k => (rc := NONE; MLton.Cont.throw (k, SOME 13)))
