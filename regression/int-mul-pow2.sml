functor Test(structure Int:
                sig
                   include INTEGER
                   val zero: int
                   val one: int
                end) =
struct

val zero = Int.zero
val one = Int.one
val two = Int.+ (one, one)
val three = Int.+ (two, one)
val four = Int.* (two, two)
val eight = Int.* (four, two)
val sixteen = Int.* (eight, two)
val thirtytwo = Int.* (sixteen, two)

val maxposint = valOf Int.maxInt
val maxnegpow2 = Int.- (Int.~ maxposint, one)
val maxpospow2 = Int.+ (Int.quot (maxposint, two), one)

fun doit k =
let
   val i = maxpospow2
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.quot (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.quot (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.quot (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.quot (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
in
   ()
end

fun loop k =
   if Int.> (k, thirtytwo)
      then ()
      else (doit k; loop (Int.+ (k, one)))

val () = loop (Int.~ thirtytwo)

fun doit k =
let
   val i = maxnegpow2
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.quot (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.quot (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.quot (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.quot (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
in
   ()
end

fun loop k =
   if Int.> (k, thirtytwo)
      then ()
      else (doit k; loop (Int.+ (k, one)))

val () = loop (Int.~ thirtytwo)

val ks = [maxnegpow2, Int.quot (maxnegpow2, two), Int.quot (maxnegpow2, four),
          Int.quot (maxnegpow2, eight), Int.quot (maxnegpow2, sixteen), Int.quot (maxnegpow2, thirtytwo),
          Int.~ four, Int.~ three, Int.~ two, Int.~ one,
          zero,
          one, two, three, four,
          Int.quot (maxpospow2, thirtytwo), Int.quot (maxpospow2, sixteen), Int.quot (maxpospow2, eight),
          Int.quot (maxpospow2, four), Int.quot (maxpospow2, two), maxpospow2]

fun doit k =
let
   val i = one
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.* (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.* (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.* (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.* (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
in
   ()
end

fun loop ks =
   case ks of
      [] => ()
    | k::ks => (doit k; loop ks)

val () = loop ks

fun doit k =
let
   val i = Int.~ one
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.* (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.* (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.* (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
   val i = Int.* (i, two)
   val () = print (concat [Int.toString k, " * ", Int.toString i, " = ",
                           (Int.toString (Int.* (k, i))
                            handle Overflow => "Overflow"), "\n"])
in
   ()
end

fun loop ks =
   case ks of
      [] => ()
    | k::ks => (doit k; loop ks)

val () = loop ks

end

structure Test8 = Test(structure Int =
                          struct
                             open Int8
                             val zero: int = 0
                             val one: int = 1
                          end)
structure Test16 = Test(structure Int =
                           struct
                              open Int16
                              val zero: int = 0
                              val one: int = 1
                           end)
structure Test32 = Test(structure Int =
                           struct
                              open Int32
                              val zero: int = 0
                              val one: int = 1
                           end)
structure Test64 = Test(structure Int =
                           struct
                              open Int64
                              val zero: int = 0
                              val one: int = 1
                           end)
