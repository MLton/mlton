fun 'a printSize (name: string, value: 'a): unit=
   (print "The size of "
    ; print name
    ; print " is = "
    ; print (Int.toString (MLton.size value))
    ; print " bytes.\n"
    ; MLton.GC.collect ())

datatype day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

val l = [1, 2, 3, 4]

val ul = [(),(),(),()]
val bl = [true, false, true, false]
val dl = [Monday, Tuesday, Thursday, Friday]

val rul = ref ul
val rbl = ref bl
val rdl = ref dl
val r0 = ref ()
val r8 = ref (Word8.fromInt 0)
val r16 = ref (Word16.fromInt 0)
val r32 = ref (Word32.fromInt 0)
val r64 = ref (Word64.fromInt 0)

val _ =
   (()
    ; printSize ("unit", ())
    ; printSize ("unit * unit", ((),()))
    ; printSize ("bool", true)
    ; printSize ("bool * bool", (true, false))
    ; printSize ("day", Monday)
    ; printSize ("day * day", (Monday, Tuesday))
    ; printSize ("a char", #"c")
    ; printSize ("a char * char", (#"c", #"d"))
    ; printSize ("a word8", Word8.fromInt 0)
    ; printSize ("a word8 * word8", (Word8.fromInt 0, Word8.fromInt 1))
    ; printSize ("a word16", Word16.fromInt 0)
    ; printSize ("a word16 * word16", (Word16.fromInt 0, Word16.fromInt 1))
    ; printSize ("a word32", Word32.fromInt 0)
    ; printSize ("a word32 * word32", (Word32.fromInt 0, Word32.fromInt 1))
    ; printSize ("a word64", Word64.fromInt 0)
    ; printSize ("a word64 * word64", (Word64.fromInt 90, Word64.fromInt 91))
    ; printSize ("a word64 * word64 * word64", (Word64.fromInt 80, Word64.fromInt 81, Word64.fromInt 82))
    ; printSize ("a word64 * word64 * word64 * word64", (Word64.fromInt 70, Word64.fromInt 71, Word64.fromInt 72, Word64.fromInt 73))
    ; printSize ("a unit list of length 4", ul)
    ; printSize ("a bool list of length 4", bl)
    ; printSize ("a day list of length 4", dl)
    ; printSize ("an int list of length 4", l)
    ; printSize ("a string of length 10", "0123456789")
    ; List.app (fn i => printSize ("a word64 array of length " ^ Int.toString i, Array.tabulate (i, fn _ => Word64.fromInt 0)))
               (List.tabulate (13, fn i => i))
    ; List.app (fn i => printSize ("a word32 array of length " ^ Int.toString i, Array.tabulate (i, fn _ => Word32.fromInt 0)))
               (List.tabulate (13, fn i => i))
    ; List.app (fn i => printSize ("a word16 array of length " ^ Int.toString i, Array.tabulate (i, fn _ => Word16.fromInt 0)))
               (List.tabulate (13, fn i => i))
    ; List.app (fn i => printSize ("a word8 array of length " ^ Int.toString i, Array.tabulate (i, fn _ => Word8.fromInt 0)))
               (List.tabulate (13, fn i => i))
    ; List.app (fn i => printSize ("a unit array of length " ^ Int.toString i, Array.tabulate (i, fn _ => ())))
               (List.tabulate (13, fn i => i))
    ; printSize ("a word64 ref", r64)
    ; printSize ("a word32 ref", r32)
    ; printSize ("a word16 ref", r16)
    ; printSize ("a word8 ref", r8)
    ; printSize ("a unit ref", r0)
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
    ; printSize ("an empty string", "")
    ; ()
    ) handle _ => (r0 := (); r8 := 0wx1; r16 := 0wx1; r32 := 0wx1; r64 := 0wx1; rul := []; rbl := []; rdl := [])

(* This is here so that the list is "useful".
 * If it were removed, then the optimizer (remove-unused-constructors)
 * would remove l entirely.
 *)
val _ = if 10 = foldl (op +) 0 l
           then ()
        else raise Fail "bug"

val _ = if ! r0 = () andalso
           ! r8 = 0wx0 andalso
           ! r16 = 0wx0 andalso
           ! r32 = 0wx0 andalso
           ! r64 = 0wx0 andalso
           ! rul = ul andalso
           ! rbl = bl andalso
           ! rdl = dl andalso
           true
           then ()
        else raise Fail "bug"
