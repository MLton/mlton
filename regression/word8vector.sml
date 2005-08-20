type word8 = Word8.word
   
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
   PS 1994-12-10, 1995-06-14 *)

(*KILL 05/11/1997 11:05. tho.:
use "auxil.sml";
*)

local
    open Word8Vector;
    fun extract (vec, s, l) = 
      Word8VectorSlice.vector (Word8VectorSlice.slice (vec, s, l))
    fun mapi f (vec, s, l) = 
      Word8VectorSlice.mapi (fn (i,x) => f (i+s,x)) (Word8VectorSlice.slice (vec, s, l))
    val i2w = Word8.fromInt;
    infix 9 sub;
in

val a = fromList (List.map i2w [0,1,2,3,4,5,6]);
val b = fromList (List.map i2w [44,55,66]);
val c = fromList (List.map i2w [0,1,2,3,4,5,6]);

fun equal (v, v') =
   let
      val n = Word8Vector.length v
      val n' = Word8Vector.length v'
      fun loop i =
         i = n
         orelse (Word8Vector.sub (v, i) = Word8Vector.sub (v', i)
                 andalso loop (i + 1))
   in
      n = n' andalso loop 0
   end

val test1:unit = tst' "test1" (fn _ => not (equal (a,b)));
val test2:unit = tst' "test2" (fn _ => equal (a, c));

val d = tabulate(100, fn i => i2w (i mod 7));

val test3:unit = tst' "test3" (fn _ => d sub 27 = i2w 6);

val test4a:unit = tst0 "test4a" ((tabulate(maxLen+1, i2w) seq "WRONG")
                            handle Overflow => "OK" | Size => "OK" | _ => "WRONG")

val test4b:unit = tst0 "test4b" ((tabulate(~1, i2w)       seq "WRONG")
                            handle Size => "OK" | _ => "WRONG")

val test4c:unit = tst' "test4c" (fn _ => length (tabulate(0, fn i => i2w (i div 0))) = 0);

val test5:unit = tst' "test5" (fn _ => length (fromList []) = 0 andalso length a = 7);

val test6a:unit = tst0 "test6a" ((c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG")
val test6b:unit = tst0 "test6b" ((c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG")
val test6c:unit = tst' "test6c" (fn _ => c sub 0 = i2w 0);

val e = concat [d, b, d];

val test7:unit = tst' "test7" (fn _ => length e = 203);

val test8:unit = tst' "test8" (fn _ => length (concat []) = 0);

val f = extract (e, 100, SOME 3);

val test9:unit = tst' "test9" (fn _ => equal (f, b));

val test9a:unit = tst' "test9a" (fn _ => equal (e, extract(e, 0, SOME (length e))) 
                                 andalso equal (e, extract(e, 0, NONE)));
val test9b:unit = tst' "test9b" (fn _ => equal (fromList [],
                                                extract(e, 100, SOME 0)));
val test9c:unit = tst0 "test9c" ((extract(e, ~1, SOME (length e))  seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9d:unit = tst0 "test9d" ((extract(e, length e + 1, SOME 0)  seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9e:unit = tst0 "test9e" ((extract(e, 0, SOME (length e+1)) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9f:unit = tst0 "test9f" ((extract(e, 20, SOME ~1)        seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9g:unit = tst0 "test9g" ((extract(e, ~1, NONE)  seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9h:unit = tst0 "test9h" ((extract(e, length e + 1, NONE)  seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9i:unit = tst' "test9i" (fn _ => equal (fromList [], extract (e, length e, SOME 0))
                                 andalso equal (fromList [], extract(e, length e, NONE)));

fun chkiter iter f vec (res', last') =
    tst' "test_chkiter" (fn _ =>
           let val last = ref (0w255:word8)
               val res = iter (fn x => (last := x; f x)) vec
           in equal (res, res') andalso !last = last' end)

fun chkiteri iter f vec (res', last') =
    tst' "test_chkiteri" (fn _ =>
           let val last = ref ~1
               val res = iter (fn (i, x) => (last := i; f x)) vec
           in equal (res, res') andalso  !last = last' end)

val test10a:unit = 
    chkiter map (fn x => 0w2*x) b (fromList [0w88,0w110,0w132], 0w66)

val test11a:unit = 
    chkiteri mapi (fn x => 0w2*x) (b, 0, NONE) (fromList [0w88,0w110,0w132], 2)
val test11b:unit = 
    chkiteri mapi (fn x => 0w2*x) (b, 1, NONE) (fromList [0w110,0w132], 2)
val test11c:unit = 
    chkiteri mapi (fn x => 0w2*x) (b, 1, SOME 0) (fromList [], ~1)
val test11d:unit = 
    chkiteri mapi (fn x => 0w2*x) (b, 1, SOME 1) (fromList [0w110], 1)
val test11e:unit = 
    chkiteri mapi (fn x => 0w2*x) (b, 3, NONE) (fromList [], ~1)

val test11f:unit =
    tst0 "test11f" ((mapi #2 (b, 0, SOME 4) seq "WRONG") 
                    handle Subscript => "OK" | _ => "WRONG")
val test11g:unit =
    tst0 "test11g" ((mapi #2 (b, 3, SOME 1) seq "WRONG") 
                    handle Subscript => "OK" | _ => "WRONG")
val test11h:unit =
    tst0 "test11h" ((mapi #2 (b, 4, SOME 0) seq "WRONG") 
                    handle Subscript => "OK" | _ => "WRONG")
val test11i:unit =
    tst0 "test11i" ((mapi #2 (b, 4, NONE) seq "WRONG") 
                    handle Subscript => "OK" | _ => "WRONG")
end;
