(* open.sml *)

(* Checks scoping rules of open. *)

structure A = struct structure B = struct val x = 1 end end
structure B = struct val x = 0.1 end

open A B

val y = B.x + 1
val z = x + 1.0;
