val _ = 1 = 2

val _ = 1.0 = 2.0 (* error *)

val f: ''a -> unit = fn _ => raise Fail "f"

val _ = f 1

val _ = f 1.0 (* error *)

datatype 'a t = T of 'a

val _ = T 1 = T 2

val _ = T 1.0 = T 2.0 (* 15 error *)

datatype 'a t = T

val _ = (T: int t) = T
   
val _ = (T: real t) = T (* 21 error *)

datatype t = T of u
withtype u = real

val _ = T 13.0 = T 14.0 (* 26 error *)

datatype t = T of u
and u = U of t

fun f (x: t) = x = x

datatype 'a t = T of 'a u
and 'a u = U of 'a

fun f (x: int t) = x = x

fun f (x: real t) = x = x (* 38 error *)

val f: 'a -> unit = fn x => (x = x; ()) (* 40 error *)
