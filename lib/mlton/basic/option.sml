structure Option: OPTION =
struct

type 'a t = 'a option

open Pervasive.Option

fun fold (opt, b, f) =
   case opt of
      NONE => b
    | SOME x => f (x, b)

fun forall (z, f) =
   case z of
      NONE => true
    | SOME x => f x
	 
fun app (opt, f) =
   case opt of
      NONE => ()
    | SOME x => f x

fun map (opt, f) =
   case opt of
      NONE => NONE
    | SOME x => SOME (f x)

fun equals (o1, o2, eq) =
   case (o1, o2) of
      (NONE, NONE) => true
    | (SOME x, SOME y) => eq (x, y)
    | _ => false

fun isNone opt =
   case opt of
      NONE => true
    | SOME _ => false

fun toString xToString opt =
   case opt of
      NONE => "None"
    | SOME x => concat ["Some (", xToString x, ")"]

fun layout layoutX opt =
   let open Layout
   in case opt of
      NONE => str "None"
    | SOME x => seq [str "Some (", layoutX x, str ")"]
   end

end
