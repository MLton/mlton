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

(* test/word8array.sml -- some test cases for Word8Array 
   PS 1994-12-21, 1995-05-11 *)

(*KILL 05/11/1997 11:05. tho.:
use "auxil.sml";
*)

val _ = print "Testing Word8Array...\n";

local
    open Word8Array 
    infix 9 sub;
    val array0 = fromList [];
    val copy = fn {src, si, len, dst, di} =>
      Word8ArraySlice.copy {src = Word8ArraySlice.slice (src, si, len),
                            dst = dst, di = di}
    val extract = fn (a, i, sz) =>
      Word8ArraySlice.vector (Word8ArraySlice.slice (a, i, sz))
in

val i2w = Word8.fromInt;

val w127 = i2w 127;

val a = fromList (map i2w [0,1,2,3,4,5,6]);
val b = fromList (map i2w [44,55,66]);
val c = fromList (map i2w [0,1,2,3,4,5,6]);

val test1:unit = tst' "test1" (fn () => a<>c);
val test2:unit = tst' "test2" 
  (fn () => 
           array(0, w127) <> array0
           andalso array(0, w127) <> tabulate(0, fn _ => w127)
           andalso tabulate(0, fn _ => w127) <> fromList []
           andalso array(0, w127) <> array(0, w127)
           andalso tabulate(0, fn _ => w127) <> tabulate(0, fn _ => w127)
           andalso fromList [] <> fromList [])

val d = tabulate(100, fn i => i2w (i mod 7))

val test3:unit = tst' "test3" (fn () => d sub 27 = i2w 6)

val test4a:unit = tst0 "test4a" ((tabulate(maxLen+1, i2w) seq "WRONG")
                            handle Overflow => "OK" | Size => "OK" | _ => "WRONG")

val test4b:unit = tst0 "test4b" ((tabulate(~1, i2w)       seq "WRONG")
                            handle Size => "OK" | _ => "WRONG")

val test4c:unit = 
    tst' "test4c" (fn () => length (tabulate(0, fn i => i2w (i div 0))) = 0);

val test5a:unit = tst' "test5a" (fn () => length (fromList []) = 0 andalso length a = 7);
val test5b:unit = tst' "test5b" (fn () => length array0 = 0);

val test6a:unit = tst0 "test6a" ((c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG")
val test6b:unit = tst0 "test6b" ((c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG")
val test6c:unit = tst' "test6c" (fn () => c sub 0 = i2w 0);

val e = array(203, i2w 0);
val _ = (copy{src=d, si=0, dst=e, di=0,        len=NONE}; 
         copy{src=b, si=0, dst=e, di=length d, len=NONE};
         copy{src=d, si=0, dst=e, di=length d + length b, len=NONE});
         
fun a2v a = extract(a, 0, NONE);
val ev = Word8Vector.concat [a2v d, a2v b, a2v d];

val test7:unit = tst' "test7" (fn () => length e = 203);

val test8a:unit = tst0 "test8a" ((update(e, ~1, w127); "WRONG")
                            handle Subscript => "OK" | _ => "WRONG")
val test8b:unit = tst0 "test8b" ((update(e, length e, w127); "WRONG")
                            handle Subscript => "OK" | _ => "WRONG")

val f = extract (e, 100, SOME 3);

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
   
val test9:unit = tst' "test9" (fn () => equal (f, a2v b));

val test9a:unit = tst' "test9a" (fn () => equal (ev, extract(e, 0, NONE))
                                 andalso equal (ev, extract(e, 0, SOME (length e))));
val test9b:unit = 
    tst' "test9b" (fn () => equal (Word8Vector.fromList [],
                                   extract(e, 100, SOME 0)));
val test9c:unit = tst0 "test9c" ((extract(e, ~1, SOME (length e))  seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9d:unit = tst0 "test9d" ((extract(e, length e+1, SOME 0) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9e:unit = tst0 "test9e" ((extract(e, 0, SOME (length e+1)) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9f:unit = tst0 "test9f" ((extract(e, 20, SOME ~1)        seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9g:unit = tst0 "test9g" ((extract(e, ~1, NONE)  seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9h:unit = tst0 "test9h" ((extract(e, length e+1, NONE) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9i:unit = 
    tst' "test9i" (fn () => equal (a2v (fromList []),
                                   extract(e, length e, SOME 0))
                   andalso equal (a2v (fromList []),
                                  extract(e, length e, NONE)));

val _ = copy{src=e, si=0, dst=e, di=0, len=NONE};
val g = array(203, w127);
val _ = copy{src=e, si=0, dst=g, di=0, len=NONE};

val test10a:unit = tst' "test10a" (fn () => equal (ev, extract(e, 0, NONE))
                              andalso equal (ev, extract(e, 0, SOME (length e))));
val test10b:unit = tst' "test10b" (fn () => equal (ev, extract(g, 0, NONE))
                              andalso equal (ev, extract(g, 0, SOME (length g))));

val _ = copy{src=g, si=203, dst=g, di=0, len=SOME 0};
val test10c:unit = tst' "test10c" (fn () => equal (ev, extract(g, 0, NONE)));

val _ = copy{src=g, si=0, dst=g, di=203, len=SOME 0};
val test10d:unit = tst' "test10d" (fn () => equal (ev, extract(g, 0, NONE)));

val _ = copy{src=g, si=0, dst=g, di=1, len=SOME (length g-1)};
val test10e:unit = tst' "test10e" (fn () => equal (a2v b, extract(g, 101, SOME 3)));

val _ = copy{src=g, si=1, dst=g, di=0, len=SOME(length g-1)};
val test10f:unit = tst' "test10f" (fn () => equal (a2v b, extract(g, 100, SOME 3)));

val _ = copy{src=g, si=202, dst=g, di=202, len=SOME 1};
val test10g:unit = tst' "test10g" (fn () => g sub 202 = i2w ((202-1-103) mod 7));
val test10h:unit = tst' "test10h" (fn () =>
                              (copy{src=array0, si=0, dst=array0, di=0, len=NONE}; 
                               array0 <> array(0, w127)));
val test10i:unit = tst' "test10i" (fn () =>
                               (copy{src=array0, si=0, dst=array0, di=0, len=SOME 0}; 
                                array0 <> array(0, w127)));

val test11a:unit = tst0 "test11a" ((copy{src=g, si= ~1, dst=g, di=0, len=NONE}; "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")
val test11b:unit = tst0 "test11b" ((copy{src=g, si=0, dst=g, di= ~1, len=NONE}; "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")
val test11c:unit = tst0 "test11c" ((copy{src=g, si=1, dst=g, di=0, len=NONE}; "OK") 
                              handle _ => "WRONG")
val test11d:unit = tst0 "test11d" ((copy{src=g, si=0, dst=g, di=1, len=NONE}; "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")
val test11e:unit = tst0 "test11e" ((copy{src=g, si=203, dst=g, di=0, len=NONE}; "OK") 
                              handle _ => "WRONG")

val test11f:unit = tst0 "test11f" ((copy{src=g, si= ~1, dst=g, di=0, len=SOME (length g)}; "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")
val test11g:unit = tst0 "test11g" ((copy{src=g, si=0, dst=g, di= ~1, len=SOME (length g)}; "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")
val test11h:unit = tst0 "test11h" ((copy{src=g, si=1, dst=g, di=0, len=SOME (length g)}; "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")
val test11i:unit = tst0 "test11i" ((copy{src=g, si=0, dst=g, di=1, len=SOME (length g)}; "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")
val test11j:unit = tst0 "test11j" ((copy{src=g, si=0, dst=g, di=0, len=SOME (length g+1)}; "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")
val test11k:unit = tst0 "test11k" ((copy{src=g, si=203, dst=g, di=0, len=SOME 1}; "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")

end;
