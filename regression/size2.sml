fun 'a printSize (name: string, value: 'a): unit=
   (print "The size of "
    ; print name
    ; print " is = "
    ; print (Int.toString (MLton.size value))
    ; print " bytes.\n")

val l = [1, 2, 3, 4]

val _ =
   (
    printSize ("a char", #"c")
    ; printSize ("an int list of length 4", l)
    ; printSize ("a string of length 10", "0123456789")
    ; printSize ("an int array of length 10", Array.tabulate (10, fn _ => 0))
    ; printSize ("a double array of length 10",
                 Array.tabulate (10, fn _ => 0.0))
    ; printSize ("a (word32 * double) array of length 10",
                 Array.tabulate (10, fn i => (Word32.fromInt i, 0.0)))
    ; printSize ("a (word32 * word32 * double) array of length 10",
                 Array.tabulate (10, fn i => (Word32.fromInt (i + 1),
                                              Word32.fromInt i, 0.0)))
    ; printSize ("a (word64 * double) array of length 10",
                 Array.tabulate (10, fn i => (Word64.fromInt (i + 1), 0.0)))
    ; printSize ("a (word16 * double) array of length 10",
                 Array.tabulate (10, fn i => (Word16.fromInt (i + 1), 0.0)))
    ; printSize ("a word64 array of length 10",
                 Array.tabulate (10, fn i => Word64.fromInt i))
    ; printSize ("a (word32 * word64) array of length 10",
                 Array.tabulate (10, fn i => (Word32.fromInt i, 
                                              Word64.fromInt i)))
    ; printSize ("a (word32 * word32 * word64) array of length 10",
                 Array.tabulate (10, fn i => (Word32.fromInt i, 
                                              Word32.fromInt (i + 1),
                                              Word64.fromInt i)))
    ; printSize ("a (word64 * word64) array of length 10",
                 Array.tabulate (10, fn i => (Word64.fromInt (i + 1),
                                              Word64.fromInt i)))
    ; printSize ("a (word16 * word64) array of length 10",
                 Array.tabulate (10, fn i => (Word16.fromInt (i + 1),
                                              Word64.fromInt i)))
    ; printSize ("an array of length 10 of 2-ples of ints",
                 Array.tabulate (10, fn i => (i, i + 1)))
    ; printSize ("an array of length 10 of 2-ples of (shared) ints",
                 let val t = (0, 1) in
                 Array.tabulate (10, fn _ => t)
                 end)
    ; printSize ("an array of length 10 of arrays of length 20 of ints",
                 Array.tabulate (10, fn i => Array.tabulate (20, fn j => i + j)))
    ; printSize ("an array of length 10 of (shared) arrays of length 20 of ints",
                 let val a = Array.tabulate (20, fn j => j)
                 in Array.tabulate (10, fn i => a)
                 end)
    ; printSize ("an array of length 10 of tuples of word16 * (arrays of length 20 of ints)",
                 Array.tabulate (10, fn i => (Word16.fromInt i, Array.tabulate (20, fn j => i + j))))
    ; printSize ("an array of length 10 of tuples of word32 * (arrays of length 20 of ints)",
                 Array.tabulate (10, fn i => (Word32.fromInt i, Array.tabulate (20, fn j => i + j))))
    ; printSize ("an array of length 10 of tuples of word64 * (arrays of length 20 of ints)",
                 Array.tabulate (10, fn i => (Word64.fromInt i, Array.tabulate (20, fn j => i + j))))
    ; printSize ("an array of length 10 of tuples of real32 * (arrays of length 20 of ints)",
                 Array.tabulate (10, fn i => (Real32.fromInt i, Array.tabulate (20, fn j => i + j))))
    ; printSize ("an array of length 10 of tuples of real64 * (arrays of length 20 of ints)",
                 Array.tabulate (10, fn i => (Real64.fromInt i, Array.tabulate (20, fn j => i + j))))
    ; printSize ("a useless function", fn _ => 13)
    )

(* This is here so that the list is "useful".
 * If it were removed, then the optimizer (remove-unused-constructors)
 * would remove l entirely.
 *)
val _ = if 10 = foldl (op +) 0 l
           then ()
        else raise Fail "bug"
