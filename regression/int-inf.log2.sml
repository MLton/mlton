val _ =
   List.app
   (fn i => print (concat [Int.toString (IntInf.log2 i), "\n"]))
   [1,
    2,
    3,
    0x10000000,
    0x20000000,
    0x40000000,
    0x80000000,
    0x100000000,
    0x1FFFFFFFF,
    0x200000000,
    0x200000001]

val _ =
   List.app
   (fn i =>
    if i = IntInf.log2 (IntInf.pow (2, i))
       andalso i = IntInf.log2 (IntInf.pow (2, i) + 1)
       andalso i - 1 = IntInf.log2 (IntInf.pow (2, i) - 1)
       then ()
    else raise Fail "bug")
   (List.tabulate (100, fn i => i + 1))

val _ = print "OK\n"
      
    


