(* The results of notb, andb and xorb can need one limb more than their
 * operands.  p = 2^4096 is a whole number of limbs, and each result below
 * is ~p, one limb longer than a = p - 1.  Without that limb reserved, the
 * debug runtime (-debug true) fails an assertion in setFrontier.
 *)

val p = IntInf.<< (1, 0w4096)
val a = p - 1

fun check (name, r) =
   print (name ^ ": " ^ (if r = ~p then "ok" else "wrong") ^ "\n")

val () = check ("notb", IntInf.notb a)
val () = check ("andb", IntInf.andb (~a, ~2))
val () = check ("xorb", IntInf.xorb (a, ~1))
