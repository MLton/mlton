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

(* sharing of vectors *)
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
