(* This example is interesting because at the time of generalization of f, the
 * tyvar 'a is in scope, but does not appear in type types of any of the
 * variables in the environment (x's type has not yet been determined to be 'a).
 * Nevertheless, it is essential to not generalize 'a at g
 *)
val 'a f = fn x =>
  let
     exception E of 'a
     fun g (E y) = y
  in
     E x
  end

(* This example is interesting because it binds a type variable at a scope where
 * the type variable does not appear in the type.  Nevertheless, it is essential
 * to keep the type variable there, because it occurs in an inner scope.
 *)
fun 'a f () =
   let
      val x: 'a = raise Fail "bug"
   in
      ()
   end
