(* This program must fail because the semicolon means that the declarations
 * must be treated as two topdecs, not a single topdec leading to two strdec's.
 * This follows from the restriction on page 14 of the Definition that states
 * "No topdec may contain as an initial segment, a strdec followed by a
 *  semicolon"
 *)
fun double x = x + x;
val y = double 2.0
