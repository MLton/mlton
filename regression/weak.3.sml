fun find cache x =
   case (List.find (fn (y,_) => x = y) (!cache)) of
      NONE => NONE
    | SOME (_,r) => SOME r
fun remove cache x =
   cache := (List.filter (fn (y,_) => not (x = y)) (!cache))
fun insert cache (x,r) =
   cache := (x,r)::(!cache)

val cache = ref []

fun lookup (x : int) =
   case find cache x of
      SOME r  => (case MLton.Weak.get r of
                     SOME r' => r'
                   | NONE => (remove cache x; lookup x))
    | NONE    => let val res = x + 1
                     val wres = MLton.Weak.new res
                 in insert cache (x, wres);
                    res
                 end

val _ = List.app (fn x => print (concat [Int.toString (lookup x), "\n"])) [5,4,3,2,1]
