(* This fails because the semicolon means that the program must be treated
 * as two topdecs.  Then, the first topdec must be elaborated, and the type
 * of x chosen, before the second can.  Hence, since we cannot know that x should
 * be an int list ref, we fail.
 *)
val x = ref [];
val _ = 1 :: !x
