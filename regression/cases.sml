fun test (f, a, r) =
   if f a = r
      then ()
   else raise Fail "bug"

val rec f =
   fn 0 => 0
    | 1 => 1
    | 2 => 2
    | i => f (i - 1)

val _ = test (f, 100, 2)

val rec f: word -> int =
   fn 0w0 => 0
    | 0w1 => 1
    | 0w2 => 2
    | w => f (w - 0w1)

val _ = test (f, 0w100, 2)

val rec f: Word8.word -> int =
   fn 0w0 => 0
    | 0w1 => 1
    | 0w2 => 2
    | w => f (w - 0w1)

val _ = test (f, 0w100, 2)

val rec f: char -> int =
   fn #"a" => 0
    | #"b" => 1
    | #"c" => 2
    | _ => f #"c"

val _ = test (f, #"d", 2)
