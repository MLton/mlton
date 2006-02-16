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


(* test/list.sml  PS 1994-12-10;  Martin-11/03/1998 *)

val _ = print "\nFile list.sml: Testing structure List...\n";

local 
    open List
in
val v123 = [1,2,3];
fun even i = i mod 2 = 0;

val test1 = tst "test1" (null [] andalso not (null [[]]));

val test2 = tst "test2" (1 = hd v123 andalso [2,3] = tl v123 andalso 3 = last v123)

val test3 = tst0 "test3" ((hd []   seq "WRONG") handle Empty => "OK" | _ => "WRONG")
val test4 = tst0 "test4" ((tl []   seq "WRONG") handle Empty => "OK" | _ => "WRONG")
val test5 = tst0 "test5" ((last [] seq "WRONG") handle Empty => "OK" | _ => "WRONG")

val test6 = tst "test6" (1 = nth(v123,0) andalso 3 = nth(v123,2))

val test7 = tst0 "test7" ((nth(v123,~1) seq "WRONG") handle Subscript => "OK" | _ => "WRONG")
val test8 = tst0 "test8" ((nth(v123,3)  seq "WRONG") handle Subscript => "OK" | _ => "WRONG")

val test9 = tst "test9" (3 = length v123);

val test10 = tst "test10" ([3,2,1] = rev [1,2,3]);

val v16 = v123 @ [4,5,6];

val test11 = tst "test11" ([1,2,3,4,5,6] = v16);

val test12 = tst "test12" (concat [] = [] andalso concat [v16] = v16 
                           andalso concat [v123, [4,5,6]] = v16);

val test13 = tst "test13"(rev v16 = revAppend([4,5,6], [3,2,1]));

local 
    val v = ref 0
    fun h [] r = r | h (x::xr) r = h xr (r+r+x): int;
    val isum = h v16 0
in 
    fun reset () = v := 0;
    fun incrv i = v := 2 * !v + i;
    fun checkv () = tst "checkv" (!v = isum);
end;

val test14 = (reset (); app incrv v16; checkv);

val test15 = tst "test15" ([2,4,6,8,10,12] = map (fn i=>i*2) v16);

val test16 = 
    tst "test16" ([3,9,15] = 
          mapPartial (fn i => if even i then NONE else SOME (3*i)) v16);

val test17 = tst "test17" (NONE = find (fn i => i>7) v16);

val test18 = tst "test18" (SOME 5 = find (fn i => i>4) v16);

val test19 = tst "test19" (NONE = find (fn _ => true) []);

val test20 = tst "test20" ([2,4,6] = filter even v16);

val test21 = (reset (); filter (fn i => (incrv i; true)) v16 seq checkv());

val test22 = tst "test22" (([2,4,6], [1,3,5]) = partition even v16);

val test23 = (reset (); partition (fn i => (incrv i; true)) v16 seq checkv());

val test24 = tst "test24" (v16 = foldr op:: [] v16);
val test25 = tst "test25" (rev v16 = foldl op:: [] v16);

val test26 = (reset(); foldr (fn (i,r) => incrv i) () (rev v16); checkv());
val test27 = (reset(); foldl (fn (i,r) => incrv i) () v16; checkv());

val test28 = tst "test28" (21 = foldr op+ 0 v16 andalso 21 = foldl op+ 0 v16);

val test29 = tst "test29" (all (fn _ => false) [] 
                   andalso not (exists (fn _ => true) [])); 

val test30 = tst "test30" (exists even [1,1,1,1,1,1,2,1] 
                   andalso all even [6,6,6,6,6,6,6,6]);

val test31 = tst "test31" (v16 = tabulate (6, fn i => i+1));

val test32 = (reset(); tabulate (6, fn i => (incrv (i+1); 127)) seq checkv());

val test33 = tst "test33" ([] = tabulate (0, fn i => 1 div i));

val test34 = tst0 "test36b" ((tabulate(~1, fn _ => raise Div) seq "WRONG") 
                             handle Size => "OK" | _ => "WRONG")

val test35a = tst "test35a" (drop([], 0) = [] 
                   andalso drop(v123, 0) = v123 
                   andalso drop(v123, 3) = []);
val test35b = tst0 "test36b" ((drop(v123, ~1) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG")
val test35c = tst0 "test35c" ((drop(v123, 4) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG")

val test36a = tst "test36a" (take([], 0) = [] 
                   andalso take(v123, 3) = v123
                   andalso take(v123, 0) = []);
val test36b = tst0 "test36b" ((take(v123, ~1) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG")
val test36c = tst0 "test36c" ((take(v123, 4) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG")

val test37a = 
    tst' "test37a" (fn _ => getItem [] = NONE
           andalso getItem [#"A"] = SOME(#"A", [])
           andalso getItem [#"B", #"C"] = SOME(#"B", [#"C"]));
end;
