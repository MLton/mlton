open MLton open Cont

val kr: unit Cont.t option ref = ref NONE

val rr: unit ref option ref = ref NONE

val r: unit ref = ref (callcc (fn k => kr := SOME k))

val _ =
   case !rr of
      NONE =>
         (rr := SOME r
          ; throw (valOf (!kr), ()))
    | SOME r' => if r = r'
                    then raise Fail "bug"
                 else ()
