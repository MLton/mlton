val f = fn x => x
and r = ref 13
val _ = (f 1; f true)
val () = r := !r + 1
val () = print (concat [Int.toString (!r), "\n"])
val () = r := !r + 1
val () = print (concat [Int.toString (!r), "\n"])
   
val x = let exception E of 'a in () end

val 'a x = let exception E of 'a in () end

val 'a id = fn x: 'a => x
and     x = let exception E of 'a in () end

val 'a _ = let exception E of 'a in E end

val 'a (f: int -> int, _) = (fn x => x, let exception E of 'a in E end);
