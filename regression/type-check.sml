(* This example is interesting because at the time of generalization of f, the
 * tyvar 'a is in scope, but does not appear in type types of any of the
 * variables in the environment (x's type has not yet been determined to be 'a).
 * Nevertheless, it is essential to not generalize 'a at g
 *)
val 'a f = fn x =>
  let
     exception E of 'a
     fun g (E y) = y
       | g _ = raise Fail "bug"
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

(* This example shows that type variables can be rebound in nested datatype
 * declarations, unlike the situation for nested value declarations.
 *)
val 'a x =
   fn () =>
   let
      datatype 'a t = T of 'a
   in
      ()
   end

(* This example verifies that datatype replication is allowed, even when the
 * right-hand side isn't a datatype.
 *)
type 'a t = 'a * 'a
datatype u = datatype t
val _: int u = (13, 14);

(* The following examples demonstrate acceptable forms of type variable scoping.
 *)
fun f (x: 'a) =
   let
      fun g (y: 'a) = y
   in
      ()
   end

fun f (x: 'a) =
   let
      fun 'b g (y: 'b) = y
   in
      ()
   end

fun 'a f (x: 'a) =
   let
      fun g (y: 'a) = y
   in
      ()
   end

fun 'a f (x: 'a) =
   let
      fun 'b g (y: 'b) = y
   in
      ()
   end

val 'a x =
   fn () =>
   let
      datatype 'a t = T of 'a
   in
      ()
   end

(* This example confirms that bools can be used as labels. *)
val {false = x, ...} = {false = 13};
