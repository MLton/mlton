structure Promise: PROMISE =
struct

datatype 'a t = T of 'a state ref
and 'a state =
   Unevaluated of unit -> 'a
  | Evaluating
  | Evaluated of 'a

fun delay th = T (ref (Unevaluated th))

fun reset (T r, th) =
   case !r of
      Evaluating => Error.bug "Promise.reset"
    | _ => r := Unevaluated th

exception Force
fun force (T r) =
   case !r of
      Evaluated x => x
    | Unevaluated th =>
	 (let
	     val _ = r := Evaluating
	     val x = th ()
	     val _ = r := Evaluated x
	  in
	     x
	  end handle exn => (r := Unevaluated th; raise exn))
    | Evaluating => raise Force

fun lazy th =
   let val p = delay th
   in fn () => force p
   end

end
