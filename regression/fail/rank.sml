local
val f = let val f = (fn x => x) (fn x => x)
        in f
        end

datatype t = A

val g = fn x => let val d = #1 x
                    val k = #2 x
                    val s = f (d,k)
                    val _ = if true then (d,k) else x
                in s
                end
in
val a : t * t = g (A,A)
end