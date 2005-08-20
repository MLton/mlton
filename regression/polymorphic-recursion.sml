datatype 'a t = R of 'a | L of 'a t t

val rec build: int -> int t =
   fn 0 => R 17
    | n => L (R (build (n - 1)))

val rec depth: int t -> int =
   fn R _ => 0
    | L (R z) => 1 + depth z
    | _ => raise Match
         
val n = depth (build 13)
val _ =
   if n = 13
      then ()
   else raise Fail "bug"

(*
val _ = R 13
val _ = L (R (R 13))
   
val rec build: int -> int t =
   fn 0 => R 13
    | n => L (R (build (n - 1)))

val _ = f 13
  *) 
(*
         
val rec f =
   fn R _ => 0
    | L (R z) => 1 + f z
         
val v0: int t = R 13
val v2: int t t = R v0
val v1: int t = L (v2: int t t)
val _ = L (L (R (R (R 13))))
val _ = L (R (L (R (R 13))))
val _ = L (L (R (R (R (R (R 13))))))

   *)

(*
datatype 'a t = A of 'a | B of ('a t * 'a t) t
val v1 = A 13
val v2 = A (v1, v1)
val v3 = A (v2, v2)
val v4 = B v3
val v5 = B v4

val x = A 1 : int t

val y = B (A (x, x))

val z = B (A (y, y))

val a = B (A (z, z))

fun d ((A _) : 'a t) : int      = 0
  | d ((B (A (x, _))) : 'a t) : int = 1 + d x

val n = d a
*)

   
(*

 Here's (the relevant part of) what the monomorphiser in smlc returns
for your program.

datatype t_0 = B_0 of t_1 | A_0 of int
     and t_1 = A_1 of (t_0 * t_0)

It figures out exactly your observation that every use of B must be
followed by A.

[z0 = int t] 
datatype z0 = A of int | B of z1

[z1 = (z0 * z0) t]
datatype z1 = A of z0 * z0 | B of z2

[z2 = (z1 * z1) t]
datatype z2 = A of z1 * z1 | B of z3

[z3 = (z2 * z2) t]

   

   
B z1
B (B z2)
B (B (A (z1, z1)))
B (B (A (A (z0,z0), A (z0,z0))))

B (B (A (A (v1, v1), A (v1, v1))))

B (B (A (A (v1, v1), A (v1, v1))))


 
datatype z = A of int | B of (z * z) t
datatype w = A of z * z | B of (w * w) t

*)
