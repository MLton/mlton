datatype ('a, 'b) either = LEFT of 'a | RIGHT of 'b

fun eval thunk =
    LEFT (thunk ()) handle e => RIGHT e

datatype 'a status = LAZY of unit -> 'a promise
                   | EAGER of ('a, exn) either
withtype 'a promise = 'a status ref ref

fun lazy exp =
    ref (ref (LAZY exp))

fun delay exp =
    lazy (fn () => ref (ref (EAGER (eval exp))))

fun force promise =
    case !(!promise)
     of EAGER (LEFT x) => x
      | EAGER (RIGHT x) => raise x
      | LAZY exp =>
        let
          val promise' = exp ()
        in
          (case !(!promise)
            of LAZY _ => (!promise := !(!promise') ;
                          promise' := !promise)
             | _ => ())
        ; force promise
        end

exception Assertion

fun check (b, e) = if b then () else raise e
fun verify b = check (b, Assertion)

val () =
    let
      val r = delay (fn () => (print "hi\n" ; 1))
      val s = lazy (fn () => r)
      val t = lazy (fn () => s)
    in
      verify (1 = force t)
    ; verify (1 = force r)
    end
