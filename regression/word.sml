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

(* test/word.sml -- some test cases for Word, appropriate for a two's
   complement machine whose Int.precision = SOME 31 
   PS 1995-03-19, 1995-07-12, 1995-11-06, 1996-04-01, 1996-10-01 

   modified to work for Int.precision = SOME 32  -- ME 1998-10-07
*)

(*KILL 05/11/1997 11:04. tho.:
use "auxil.sml";
*)

local 
    (* Isn't this disgusting: *)
    val [gt,  lt,  ge,   le] = 
        [op>, op<, op>=, op<=] : (int * int -> bool) list
    val [add, sub, mul, idiv,   imod] = 
        [op+, op-, op*, op div, op mod] : (int * int -> int) list
    open Word;
    val op > = gt and op < = lt and op >= = ge and op <= = le;
    val op + = add and op - = sub and op * = mul 
    and op div = idiv and op mod = imod;
    val i2w = fromInt
    and w2i = toIntX;
    fun pr_ln s s' = print (s ^ ": " ^ s' ^ "\n")
in

val test1 = checkrange (0, 1025)
    (fn i => i = w2i (i2w i));
val _ = pr_ln "test1" test1

val test3 = checkrange (~1000, 1000) 
    (fn i => i = toIntX (i2w i));
val _ = pr_ln "test3" test3

val test5a = checkrange (0,15) 
    (fn i => (i+960) div 2 * 2 + 1
             = w2i (orb (i2w i, i2w 961)));
val _ = pr_ln "test5a" test5a
val test5b = checkrange (0,513)
    (fn i => i = w2i (orb (i2w i, i2w i)));
val _ = pr_ln "test5b" test5b
val test6a = checkrange (0,15) 
    (fn i => i div 2 * 2 = w2i (andb (i2w i, i2w ~2)));
val _ = pr_ln "test6a" test6a
val test6b = checkrange (0,513)
    (fn i => i = w2i (andb (i2w i, i2w i)));
val _ = pr_ln "test6b" test6b
val test7a = checkrange (0,15) 
    (fn i => i+960 = w2i (xorb (i2w i, i2w 960)));
val _ = pr_ln "test7a" test7a
val test7b = checkrange (0, 513)
    (fn i => 0 = w2i (xorb (i2w i, i2w i)));
val _ = pr_ln "test7b" test7b
val test8a = check(~1 = w2i (notb (i2w 0)));
val _ = pr_ln "test8a" test8a
val test8b = check (0 = w2i (notb (i2w ~1)));
val _ = pr_ln "test8b" test8b
val maxposint = valOf Int.maxInt;
val maxnegint = (Int.~ maxposint)-1;
fun pwr2 0 = 1 
  | pwr2 n = 2 * pwr2 (n-1);
fun rwp i 0 = i
  | rwp i n = rwp i (n-1) div 2;

val test9a = checkrange (0,29)
    (fn k => pwr2 k = w2i (<< (i2w 1, i2w k)));
val _ = pr_ln "test9a" test9a
val test9b = checkrange (32,65)
    (fn k => 0 = w2i (<< (i2w 1, i2w k)));
val _ = pr_ln "test9b" test9b
val test9c = check (maxnegint = w2i (<< (i2w 1, i2w 31)));
val _ = pr_ln "test9c" test9c
val test9d = checkrange (0, 1025)
    (fn i => 2 * i = w2i (<< (i2w i, i2w 1)));
val _ = pr_ln "test9d" test9d
val test9e = checkrange (0, 1025)
    (fn i => i div 2 = w2i (>> (i2w i, i2w 1)));
val _ = pr_ln "test9e" test9e
val test9f = checkrange (0,65)
    (fn k => rwp maxposint k = w2i (>> (i2w maxposint, i2w k)));
val _ = pr_ln "test9f" test9f
val test9g = checkrange (32,65)
    (fn k => 0 = w2i (<< (i2w ~1, i2w k)));
val _ = pr_ln "test9g" test9g
val test9h = checkrange (1,65)
    (fn k => 0 = w2i (>> (i2w 1, i2w k)));
val _ = pr_ln "test9h" test9h

val test10a = checkrange (1,65)
    (fn k => 0 = w2i (~>> (i2w 1, i2w k)));
val _ = pr_ln "test10a" test10a
val test10b = checkrange (1,65)
    (fn k => ~1 = w2i (~>> (i2w ~1, i2w k)));
val _ = pr_ln "test10b" test10b
val test10c = checkrange (~513, 513)
    (fn i => i div 2 = toIntX (~>> (i2w i, i2w 1)));
val _ = pr_ln "test10c" test10c
val test10d = checkrange (0,65)
    (fn k => rwp maxnegint k = toIntX (~>> (i2w maxnegint, i2w k)));
val _ = pr_ln "test10d" test10d
local 
    open Word
in
val test11a = check (i2w 256 > i2w 255);
val _ = pr_ln "test11a" test11a
val test11b = check (i2w 0 < i2w ~1);
val _ = pr_ln "test11b" test11b
val test11c = check (i2w maxposint >= i2w maxposint);
val _ = pr_ln "test11c" test11c
val test11d = check (i2w maxnegint >= i2w 127);
val _ = pr_ln "test11d" test11d
val test11e = check (i2w 1 <= i2w 1);
val _ = pr_ln "test11e" test11e
val test11f = check (i2w 0 <= i2w 1);
val _ = pr_ln "test11f" test11f
val test11g = check (i2w 0 < i2w maxposint);
val _ = pr_ln "test11g" test11g
val test11h = check (i2w maxposint < i2w maxnegint);
val _ = pr_ln "test11h" test11h
val test11i = check (i2w maxnegint < i2w ~1);
val _ = pr_ln "test11i" test11i
end;

local 
    open Word
in
val test12a = checkrange(0, 300) (fn k => w2i (i2w k + i2w 17) = add(k, 17));
val _ = pr_ln "test12a" test12a
val test12b = checkrange(0, 300) (fn k => w2i (i2w k - i2w 17) = sub(k, 17));
val _ = pr_ln "test12b" test12b
val test12c = checkrange(0, 300) (fn k => w2i (i2w k * i2w 17) = mul(k, 17));
val _ = pr_ln "test12c" test12c
val test12d = checkrange(0, 300) 
    (fn k => w2i (i2w k div i2w 17) = idiv(k, 17));
val _ = pr_ln "test12d" test12d
val test12e = checkrange(0, 300) 
    (fn k => w2i (i2w k mod i2w 17) = imod(k, 17));
val _ = pr_ln "test12e" test12e
val test12f = checkrange(0, 300) 
    (fn k => w2i (i2w k + i2w maxnegint) = add(k, maxnegint));
val _ = pr_ln "test12f" test12f
val test12g = checkrange(0, 300) 
    (fn k => w2i (i2w maxnegint - i2w k - i2w 1) = sub(maxposint,k));
val _ = pr_ln "test12g" test12g
val test12h = checkrange(0, 300) 
    (fn k => w2i (i2w k * i2w maxnegint) = mul(imod(k, 2), maxnegint));
val _ = pr_ln "test12h" test12h
val test12i = checkrange(0, 300) 
    (fn k => w2i (i2w k * i2w maxposint + i2w k) = mul(imod(k, 2), maxnegint));
val _ = pr_ln "test12i" test12i
val test12j = checkrange(0, 300) 
    (fn k => w2i (i2w k div i2w ~1) = 0);
val _ = pr_ln "test12j" test12j
val test12k = checkrange(0, 300) 
    (fn k => w2i (i2w k mod i2w ~1) = k);
val _ = pr_ln "test12k" test12k
val test12l = check(w2i (i2w maxposint + i2w 1) = maxnegint);
val _ = pr_ln "test12l" test12l
val test12m = check(w2i (i2w maxnegint - i2w 1) = maxposint);
val _ = pr_ln "test12m" test12m
val test12n = check(w2i (i2w ~1 div i2w 2) = maxposint);
val _ = pr_ln "test12n" test12n
val test12o = check(w2i (i2w ~1 mod i2w 2) = 1);
val _ = pr_ln "test12o" test12o
val test12p = check(w2i (i2w ~1 div i2w 100) = idiv(maxposint, 50));
val _ = pr_ln "test12p" test12p
(*31bit
val test12q = check(w2i (i2w ~1 mod i2w 10) = 7);
val _ = pr_ln "test12q" test12q
*)
val test12r = (i2w 17 div i2w 0 seq "WRONG") 
              handle Div => "OK" | _ => "WRONG";
val _ = pr_ln "test12r" test12r
val test12s = (i2w 17 mod i2w 0 seq "WRONG") 
              handle Div => "OK" | _ => "WRONG";
val _ = pr_ln "test12s" test12s
fun chk f (s, r) = 
    check'(fn _ => 
           case f s of
               SOME res => res = i2w r
             | NONE     => false)

fun chkScan fmt = chk (StringCvt.scanString (scan fmt))

val test13a = 
    List.map (chk fromString)
             [("20Af", 8367),
              (" \n\t20AfGrap", 8367),
              ("0w20Af", 0 (*8367*)),
              (" \n\t0w20AfGrap", 0 (*8367*)),
              ("0", 0),
              ("0w", 0),
              ("0W1", 0),
              ("0w ", 0),
              ("0wx", 0),
              ("0wX", 0),
              ("0wx1", 1),
              ("0wX1", 1),
              ("0wx ", 0),
              ("0wX ", 0)];
val _ = pr_ln "test13a" (concat test13a)
val test13b = 
    List.map (fn s => case fromString s of NONE => "OK" | _ => "WRONG")
           ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
            "+1", "~1", "-1", "GG"];        
val _ = pr_ln "test13b" (concat test13b)

val test14a = 
    List.map (chkScan StringCvt.DEC)
             [("10789", 10789),
              (" \n\t10789crap", 10789),
              ("0w10789", 10789),
              (" \n\t0w10789crap", 10789),
              ("0", 0),
              ("0w", 0),
              ("0W1", 0),
              ("0w ", 0),
              ("0wx", 0),
              ("0wX", 0),
              ("0wx1", 0),
              ("0wX1", 0),
              ("0wx ", 0),
              ("0wX ", 0)];
val _ = pr_ln "test14a" (concat test14a)
val test14b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.DEC) s 
                      of NONE => "OK" | _ => "WRONG")
           ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
            "+1", "~1", "-1", "ff"];        
val _ = pr_ln "test14b" (concat test14b)
val test15a = 
    List.map (chkScan StringCvt.BIN)
             [("10010", 18),
              (" \n\t10010crap", 18),
              ("0w10010", 18),
              (" \n\t0w10010crap", 18),
              ("0", 0),
              ("0w", 0),
              ("0W1", 0),
              ("0w ", 0),
              ("0wx", 0),
              ("0wX", 0),
              ("0wx1", 0),
              ("0wX1", 0),
              ("0wx ", 0),
              ("0wX ", 0)];
val _ = pr_ln "test15a" (concat test15a)
val test15b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.BIN) s 
                      of NONE => "OK" | _ => "WRONG")
           ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
            "+1", "~1", "-1", "2", "8", "ff"];
val _ = pr_ln "test15b" (concat test15b)
val test16a = 
    List.map (chkScan StringCvt.OCT)
             [("2071", 1081),
              (" \n\t2071crap", 1081),
              ("0w2071", 1081),
              (" \n\t0w2071crap", 1081),
              ("0", 0),
              ("0w", 0),
              ("0W1", 0),
              ("0w ", 0),
              ("0wx", 0),
              ("0wX", 0),
              ("0wx1", 0),
              ("0wX1", 0),
              ("0wx ", 0),
              ("0wX ", 0)];
val _ = pr_ln "test16a" (concat test16a)
val test16b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.OCT) s 
                      of NONE => "OK" | _ => "WRONG")
           ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
            "+1", "~1", "-1", "8", "ff"];
val _ = pr_ln "test16b" (concat test16b)
val test17a = 
    List.map (chkScan StringCvt.HEX)
             [("20Af", 8367), (" \n\t20AfGrap", 8367),
              ("0wx20Af", 8367), (" \n\t0wx20AfGrap", 8367),
              ("0wX20Af", 8367), (" \n\t0wX20AfGrap", 8367),
              ("0x20Af", 8367), (" \n\t0x20AfGrap", 8367),
              ("0X20Af", 8367), (" \n\t0X20AfGrap", 8367),
              ("0", 0),
              ("0w", 0),
              ("0w ", 0),
              ("0w1", 0 (*1*)),
              ("0W1", 0),
              ("0wx", 0),
              ("0wX", 0),
              ("0wx1", 1),
              ("0wX1", 1)];
val _ = pr_ln "test17a" (concat test17a)
val test17b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.HEX) s 
                      of NONE => "OK" | _ => "WRONG")
           ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
            "+1", "~1", "-1"];
val _ = pr_ln "test17b" (concat test17b)
end;

local 
    fun fromToString i = 
        fromString (toString (fromInt i)) = SOME (fromInt i);

    fun scanFmt radix i = 
        let val w = fromInt i
            val s = fmt radix w
        in StringCvt.scanString (scan radix) s = SOME w end;

in
val test18 = 
    check'(fn _ => range (0, 1200) fromToString);
val _ = pr_ln "test18" test18
val test19 = 
    check'(fn _ => range (0, 1200) (scanFmt StringCvt.BIN));
val _ = pr_ln "test19" test19
val test20 = 
    check'(fn _ => range (0, 1200) (scanFmt StringCvt.OCT));
val _ = pr_ln "test20" test20
val test21 = 
    check'(fn _ => range (0, 1200) (scanFmt StringCvt.DEC));
val _ = pr_ln "test21" test21
val test22 = 
    check'(fn _ => range (0, 1200) (scanFmt StringCvt.HEX));
val _ = pr_ln "test22" test22
end
end;
