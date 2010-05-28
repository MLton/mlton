fun f x =
   if x = 0
      then raise Fail "ok"
   else f (x - 1) handle Overflow => 13

val _ = (f 10; ()) handle e => (List.app (fn s => print (concat [s, "\n"]))
                                (SMLofNJ.exnHistory e))
