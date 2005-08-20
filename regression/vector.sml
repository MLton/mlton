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

(* test/vector.sml -- some test cases for Vector 
   PS 1994-12-10, 1995-06-14, 1997-03-07 *)

(*KILL 05/11/1997 11:04. tho.:
use "auxil.sml";
*)

val _ = print "Testing Vector...\n";
fun prtest (s, s') = print(s ^ ": " ^ s' ^ "\n")
local
    open Vector;
    infix 9 sub;
    fun extract (vec, s, l) = VectorSlice.vector (VectorSlice.slice (vec, s, l))
    fun mapi f (vec, s, l) = 
      VectorSlice.mapi (fn (i,x) => f (i+s,x)) (VectorSlice.slice (vec, s, l))
in

val a = fromList [0,1,2,3,4,5,6];
val b = fromList [44,55,66];
val c = fromList [0,1,2,3,4,5,6];

val test1 = check'(fn _ => a<>b);
val _ = prtest("test1", test1);
val test2 = check'(fn _ => a=c);
val _ = prtest("test2", test2);
val d = tabulate(100, fn i => i mod 7);

val test3 = check'(fn _ => d sub 27 = 6);
val _ = prtest("test3", test3);
val test4a = (tabulate(maxLen+1, fn i => i) seq "WRONG")
             handle Overflow => "OK" | Size => "OK" | _ => "WRONG";
val _ = prtest("test4a", test4a);
val test4b = (tabulate(~1, fn i => i)       seq "WRONG")
             handle Size => "OK" | _ => "WRONG";
val _ = prtest("test4b", test4b);
val test4c = check'(fn _ => length (tabulate(0, fn i => i div 0)) = 0);
val _ = prtest("test4c", test4c);
val test5 = check'(fn _ => length (fromList []) = 0 andalso length a = 7);
val _ = prtest("test5", test5);
val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = prtest("test6a", test6a);
val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = prtest("test6b", test6b);
val test6c = check'(fn _ => c sub 0 = 0);
val _ = prtest("test6c", test6c);
val e = concat [d, b, d];

val test7 = check'(fn _ => length e = 203);
val _ = prtest("test7", test7);
val test8 = check'(fn _ => length (concat []) = 0);
val _ = prtest("test8", test8);
val f = extract (e, 100, SOME 3);

val test9 = check'(fn _ => f = b);
val _ = prtest("test9", test9);
val test9a = check'(fn _ => e = extract(e, 0, SOME (length e)) 
                    andalso e = extract(e, 0, NONE));
val _ = prtest("test9a", test9a);
val test9b = check'(fn _ => fromList [] = extract(e, 100, SOME 0));
val _ = prtest("test9b", test9b);
val test9c = (extract(e, ~1, SOME (length e))  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val _ = prtest("test9c", test9c);
val test9d = (extract(e, length e + 1, SOME 0)  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val _ = prtest("test9d", test9d);
val test9e = (extract(e, 0, SOME (length e+1)) seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val _ = prtest("test9e", test9e);
val test9f = (extract(e, 20, SOME ~1)        seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val _ = prtest("test9f", test9f);
val test9g = (extract(e, ~1, NONE)  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val _ = prtest("test9g", test9g);
val test9h = (extract(e, length e + 1, NONE)  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val _ = prtest("test9h", test9h);
val test9i = check'(fn _ => fromList [] = extract(e, length e, SOME 0)
                    andalso fromList [] = extract(e, length e, NONE));
val _ = prtest("test9i", test9i);
fun chkiter iter f vec reslast =
    check'(fn _ =>
           let val last = ref ~1
               val res = iter (fn x => (last := x; f x)) vec
           in (res, !last) = reslast end)

fun chkiteri iter f vec reslast =
    check'(fn _ =>
           let val last = ref ~1
               val res = iter (fn (i, x) => (last := i; f x)) vec
           in (res, !last) = reslast end)

val test10a = 
    chkiter map (fn x => 2*x) b (fromList [88,110,132], 66)
val _ = prtest("test10a", test10a);
val test11a = 
    chkiteri mapi (fn x => 2*x) (b, 0, NONE) (fromList [88,110,132], 2)
val _ = prtest("test11a", test11a);
val test11b = 
    chkiteri mapi (fn x => 2*x) (b, 1, NONE) (fromList [110,132], 2)
val _ = prtest("test11b", test11b);
val test11c = 
    chkiteri mapi (fn x => 2*x) (b, 1, SOME 0) (fromList [], ~1)
val _ = prtest("test11c", test11c);
val test11d = 
    chkiteri mapi (fn x => 2*x) (b, 1, SOME 1) (fromList [110], 1)
val _ = prtest("test11d", test11d);
val test11e = 
    chkiteri mapi (fn x => 2*x) (b, 3, NONE) (fromList [], ~1)
val _ = prtest("test11e", test11e);
val test11f =
    (mapi #2 (b, 0, SOME 4) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val _ = prtest("test11f", test11f);
val test11g =
    (mapi #2 (b, 3, SOME 1) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val _ = prtest("test11g", test11g);
val test11h =
    (mapi #2 (b, 4, SOME 0) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val _ = prtest("test11h", test11h);
val test11i =
    (mapi #2 (b, 4, NONE) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val _ = prtest("test11i", test11i);
end;


