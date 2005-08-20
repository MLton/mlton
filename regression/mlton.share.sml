(* sharing on a non-object *)
val () = MLton.share 13
   
(* tuple option array *)
val a = Array.tabulate (100, fn i => SOME (i mod 2, i mod 3))
val () = Array.update (a, 0, NONE)

fun msg () =
   (print (concat ["size of a is ", Int.toString (MLton.size a), "\n"])
    ; Array.appi (fn (i, z) =>
                  print (concat [Int.toString i, " => ",
                                 case z of
                                    NONE => "NONE"
                                  | SOME (a, b) => 
                                       concat ["(", Int.toString a, ", ",
                                               Int.toString b, ")"],
                                 "\n"])) a)

val () = msg ()
val () = MLton.share a
val () = msg ()

(* tuple option array with pre-existing sharing *)
val a = Array.tabulate (100, fn i =>
                        if i mod 2 = 0
                           then SOME (1, 1)
                        else SOME (i mod 3, i mod 3))
val () = Array.update (a, 0, NONE)
fun msg () =
   (print (concat ["size of a is ", Int.toString (MLton.size a), "\n"])
    ; Array.appi (fn (i, z) =>
                  print (concat [Int.toString i, " => ",
                                 case z of
                                    NONE => "NONE"
                                  | SOME (a, b) => 
                                       concat ["(", Int.toString a, ", ",
                                               Int.toString b, ")"],
                                       "\n"])) a)
val () = msg ()
val () = MLton.share a
val () = msg ()

(* tuple option ref array *)
   
val a = Array.tabulate (100, fn i => ref (SOME (i mod 2, i mod 3)))
val () = Array.sub (a, 0) := NONE

fun msg () =
   (print (concat ["size of a is ", Int.toString (MLton.size a), "\n"])
    ; Array.appi (fn (i, z) =>
                  print (concat [Int.toString i, " => ",
                                 case !z of
                                    NONE => "NONE"
                                  | SOME (a, b) => 
                                       concat ["(", Int.toString a, ", ",
                                               Int.toString b, ")"],
                                 "\n"])) a)

val () = msg ()
val () = MLton.share a
val () = msg ()
val () = Array.appi (fn (i, r) =>
                     r := (if i = 0 then NONE else (SOME (i mod 2, i mod 3)))) a
val () = msg ()

(* big tuple option array *)
val a = Array.tabulate (100000, fn i => SOME (i mod 2, i mod 3))
val () = Array.update (a, 0, NONE)

fun msg () =
   print (concat ["size of a is ", Int.toString (MLton.size a), "\n",
                  case Array.sub (a, 1) of
                     NONE => "NONE"
                   | SOME (a, b) => 
                        concat ["(", Int.toString a, ", ", Int.toString b, ")"],
                        "\n"])
   
val () = msg ()
val () = MLton.share a
val () = msg ()

(* non-sharing of vectors *)
datatype t = A | B
val v1 = Vector.fromList [A, B, A, B, A, B, A, B, A, B, A, B]
val v2 = Vector.fromList [A, B, A, B, A, B, A, B, A, B, A, A]

val a = Array.tabulate (4, fn i =>
                        if i mod 2 = 0
                           then v1
                        else v2)

val () = MLton.share a

val () =
   if Array.sub (a, 2) = Array.sub (a, 3)
      then raise Fail "bug"
   else ()

(* sharing of vectors *)
val a =
   Array.tabulate (10, fn i =>
                   if i mod 2 = 0
                     then "abcdef"
                   else concat ["abc", "def"])

fun p () = print (concat ["size is ", Int.toString (MLton.size a), "\n"])

val () = p ()
   
val () = MLton.share a

val () = p ()

val s0 = Array.sub (a, 0)

val s1 = Array.sub (a, 1)

val () = print (concat [s0, " ", s1, "\n"])

(* sharing of vectors in a tuple *)
   
val t = ("abcdef", concat ["abc", "def"])

fun p () = print (concat ["size is ", Int.toString (MLton.size t), "\n"])

val () = p ()
   
val () = MLton.share t

val () = p ()

val (s1, s2) = t

val () = print (concat [s1, " ", s2, "\n"])

(* non-sharing of similar looking strings of different lengths. *)
val a =
   Array.tabulate (10, fn i =>
                   if 0 = i mod 2
                      then "a"
                   else concat ["a", "\000"])

val () = MLton.share a

val s0 = Array.sub (a, 0)
val s1 = Array.sub (a, 1)

val () =
   print (concat [Int.toString (size s0), " ",
                  Int.toString (size s1), "\n"])
   
