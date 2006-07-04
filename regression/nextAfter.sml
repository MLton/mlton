functor Test (R: REAL) =
struct

open R

val posZero = minPos - minPos
val negZero = ~posZero

val rs =
   [(negZero, negZero, negZero),
    (negZero, posZero, posZero),
    (posZero, negZero, negZero),
    (posZero, posZero, posZero),
    (negZero, posInf, minPos),
    (negZero, negInf, ~minPos),
    (posZero, posInf, minPos),
    (posZero, negInf, ~minPos),
    (minPos, posZero, posZero),
    (minPos, negZero, posZero),
    (minPos, negInf, posZero),
    (~minPos, posZero, negZero),
    (~minPos, negZero, negZero),
    (~minPos, posInf, negZero),
    (minPos, minPos, minPos),
    (~minPos, ~minPos, ~minPos),
    (minPos, posInf, fromInt 2 * minPos),
    (~minPos, negInf, ~(fromInt 2 * minPos)),
    (maxFinite, posInf, posInf),
    (~maxFinite, negInf, negInf),
    (nextAfter (minNormalPos, negInf), posInf, minNormalPos)]

val () =
   List.app
   (fn (x, y, z) =>
    let
       val r2s = fmt StringCvt.EXACT
       val z' = nextAfter (x, y)
    in
       print (concat ["nextAfter (", r2s x, ", ", r2s y, ") = ", r2s z', " ",
                      if == (z, z') then "OK" else concat ["<> ", r2s z],
                      "\n"])
    end)
   rs

end

structure Z = Test (Real32)
structure Z = Test (Real64)
   
