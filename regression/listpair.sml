(* Auxiliary functions for test cases *)

infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int 
    in
        (from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds = check o range bounds;

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");
fun tst  s b = tst0 s (check  b);
fun tst' s f = tst0 s (check' f);

fun tstrange s bounds = (tst s) o range bounds  


(* test/listpair.sml
   PS 1995-02-25, 1997-03-07
*)

(*KILL 05/11/1997 11:00. tho.:
use "auxil.sml";
*)

val _ = print "\nFile listpair.sml: Testing structure ListPair...\n";

local 
    open ListPair
    val a = [1, 2, 3, 4, 5, 6]
    val b = [10, 40, 50, 50]
    val ab = [(1, 10), (2, 40), (3, 50), (4, 50)]
    fun take 0 xs        = []
      | take n []        = []
      | take n (x :: xr) = x :: take (n-1) xr
in 

val test1 = tst "test1" (zip([], []) = [] 
                  andalso zip ([], a) = [] 
                  andalso zip(a, []) = []
                  andalso zip(a, b) = ab
                  andalso zip(b, a) = List.map (fn (x,y) => (y,x)) ab);

val test2a = tst "test2a" (([], []) = unzip []
                   andalso (a, a) = unzip(zip(a,a))
                   andalso (take (length b) a, b) = unzip(zip(a, b))
                   andalso (b, take (length b) a) = unzip(zip(b, a)));
val test2b = tst "test2b" (ab = zip(unzip ab));

val test3a = tst "test3a" (map (fn (x, y) => x + y) (a, b) = 
                  List.map (fn (x,y) => x + y) (zip(a, b)));

local 
    val v = ref 0
    fun h [] r = r | h (x::xr) r = h xr (r+r+x): int;
    val isum = h (take (length b) a) 0
in 
    fun reset () = v := 0;
    fun incrv i = v := 2 * !v + i;
    fun checkv () = tst "checkv" (!v = isum);
end;

val test3b = (reset (); map (incrv o #1) (a, b) seq (); checkv());

val test4 = (reset (); app (incrv o #1) (a, b); checkv());

val test5a = tst "test5a" (all (fn _ => false) (a, [])
                   andalso not (exists (fn _ => true) ([], b))); 

val test5b = tst "test5b" (exists (fn (x, y) => x = 3) (a, b) 
                   andalso all (fn (x, y) => y <= 50) (a, b));

val test5c = tst "test5c" (not (exists (fn (x, y) => x = 5) (a, b))
                   andalso not (exists (fn (x, y) => y = 5) (b, a))
                   andalso all (fn (x, y) => x <> 6) (a, b)
                   andalso all (fn (x, y) => y <> 6) (b, a));

val test5d = (reset(); all (fn (x,y) => (incrv x; true)) (a, b) seq (); 
              checkv());
val test5e = (reset(); exists (fn (x,y) => (incrv x; false)) (a, b) seq (); 
              checkv());

local 
    fun foldrchk f e xs ys = 
        foldr f e (xs, ys) = 
        List.foldr (fn ((x, y), r) => f(x, y, r)) e (zip(xs, ys))
    fun foldlchk f e xs ys = 
        foldl f e (xs, ys) = 
        List.foldl (fn ((x, y), r) => f(x, y, r)) e (zip(xs, ys))
in
val test6 = tst' "test6" (fn _ => 
    foldrchk (fn (x, y, (r1, r2)) => (x-r1, y div r2)) (0, 10) a b
    andalso foldrchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) [] b
    andalso foldrchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) a  []
    andalso foldrchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) [] []);

val test7 = tst' "test7" (fn _ => 
    foldlchk (fn (x, y, (r1, r2)) => (x-r1, y div r2)) (0, 10) a b
    andalso foldlchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) [] b
    andalso foldlchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) a  []
    andalso foldlchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) [] []);
end

end;

