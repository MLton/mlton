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

(* test/word8.sml -- some test cases for Word8, appropriate for a
   two's complement machine whose Int.precision = SOME 31 
   PS 1995-03-30, 1995-07-12, 1995-11-06, 1996-04-01, 1996-10-01 

   modified to work for Int.precision = SOME 32  -- ME 1998-10-07
*)

(*KILL 05/11/1997 11:05. tho.:
use "auxil.sml";
*)

local 
    fun pwr2 0 = 1 
      | pwr2 n = 2 * pwr2 (n-1);
    fun rwp i 0 = i
      | rwp i n = rwp i (n-1) div 2;

    (* Isn't this disgusting: *)
    val [gt,  lt,  ge,   le] = 
	[op>, op<, op>=, op<=] : (int * int -> bool) list
    val [add, sub, mul, idiv,   imod] = 
	[op+, op-, op*, op div, op mod] : (int * int -> int) list
    open Word8;
    val op > = gt and op < = lt and op >= = ge and op <= = le;
    val op + = add and op - = sub and op * = mul 
    and op div = idiv and op mod = imod;
    val i2W = Word.fromInt
    val i2w = fromInt
    and w2i = toInt
    fun pr_ln s s' = print (s ^ ": " ^ s' ^ "\n")
in

val test1 = checkrange (0, 255) 
    (fn i => i = w2i (i2w i));
val _ = pr_ln "test1" test1
val test2 = checkrange (~1000, 1000) 
    (fn i => let val r = w2i (i2w i) 
	     in 0 <= r andalso r < 256 end);
val _ = pr_ln "test2" test2
val test3 = checkrange (~128, 127) 
    (fn i => i = toIntX (i2w i));
val _ = pr_ln "test3" test3
val test4 = checkrange (~1000, 1000)
    (fn i => let val r = toIntX (i2w i) 
	     in ~128 <= r andalso r < 128 end);
val _ = pr_ln "test4" test4
val test5a = checkrange (0,15) 
    (fn i => (i+240) div 2 * 2 + 1
             = w2i (orb (i2w i, i2w 241)));
val _ = pr_ln "test5a" test5a
val test5b = checkrange (0,255)
    (fn i => i = w2i (orb (i2w i, i2w i)));
val _ = pr_ln "test5b" test5b
val test5c = checkrange (~1000,1000)
    (fn i => let val r = w2i (andb (i2w 2047, i2w i)) 
	     in 0 <= r andalso r < 256 end);
val _ = pr_ln "test5c" test5c
val test6a = checkrange (0,15) 
    (fn i => i div 2 * 2 = w2i (andb (i2w i, i2w 254)));
val _ = pr_ln "test6a" test6a
val test6b = checkrange (0,255)
    (fn i => i = w2i (andb (i2w i, i2w i)));
val _ = pr_ln "test6b" test6b
val test6c = checkrange (~1000,1000)
    (fn i => let val r = w2i (andb (i2w 2047, i2w i)) 
	     in 0 <= r andalso r < 256 end);
val _ = pr_ln "test6c" test6c
val test7a = checkrange (0,15) 
    (fn i => i+240 = w2i (xorb (i2w i, i2w 240)));
val _ = pr_ln "test7a" test7a
val test7b = checkrange (0, 255)
    (fn i => 0 = w2i (xorb (i2w i, i2w i)));
val _ = pr_ln "test7b" test7b
val test7c = checkrange (~1000,1000)
    (fn i => let val r = w2i (xorb (i2w 0, i2w i)) 
	     in 0 <= r andalso r < 256 end);
val _ = pr_ln "test7c" test7c
val test8a = check (255 = w2i (notb (i2w 0)));
val _ = pr_ln "test8a" test8a
val test8b = check (0 = w2i (notb (i2w 255)));
val _ = pr_ln "test8b" test8b
val test8c = checkrange (~1000,1000)
    (fn i => let val r = w2i (notb (i2w i)) 
	     in 0 <= r andalso r < 256 end);
val _ = pr_ln "test8c" test8c

val test9a = checkrange (0,7)
    (fn k => pwr2 k = w2i (<< (i2w 1, i2W k)));
val _ = pr_ln "test9a" test9a
val test9b = checkrange (8,70)
    (fn k => 0 = w2i (<< (i2w 1, i2W k)));
val _ = pr_ln "test9b" test9b
val test9c = checkrange (~50,50)
    (fn k => let val r = w2i (<< (i2w 1, i2W k))
	     in 0 <= r andalso r < 256 end);
val _ = pr_ln "test9c" test9c
val test9d = checkrange (~50,50)
    (fn k => let val r = w2i (>> (i2w 1, i2W k))
	     in 0 <= r andalso r < 256 end);
val _ = pr_ln "test9d" test9d
val test9e = checkrange (0, 127) 
    (fn i => 2 * i = w2i (<< (i2w i, i2W 1)));
val _ = pr_ln "test9e" test9e
val test9f = checkrange (0, 255)
    (fn i => i div 2 = w2i (>> (i2w i, i2W 1)));
val _ = pr_ln "test9f" test9f
val test9g = checkrange (0,64)
    (fn k => rwp 255 k = w2i (>> (i2w 255, i2W k)));
val _ = pr_ln "test9g" test9g
val test9h = checkrange (8,65)
    (fn k => 0 = w2i (<< (i2w 255, i2W k)));
val _ = pr_ln "test9h" test9h
val test9i = checkrange (1,65)
    (fn k => 0 = w2i (>> (i2w 1, i2W k)));
val _ = pr_ln "test9i" test9i

val test10a = checkrange (1,65)
    (fn k => 0 = w2i (~>> (i2w 1, i2W k)));
val _ = pr_ln "test10a" test10a
val test10b = checkrange (1,65)
    (fn k => 255 = w2i (~>> (i2w ~1, i2W k)));
val _ = pr_ln "test10b" test10b
val test10c = checkrange (~50,50)
    (fn k => let val r = w2i (~>> (i2w 1, i2W k))
	     in 0 <= r andalso r < 256 end);
val _ = pr_ln "test10c" test10c
val test10d = checkrange (~128, 127)
    (fn i => i div 2 = toIntX (~>> (i2w i, i2W 1)));
val _ = pr_ln "test10d" test10d
val test10e = checkrange (0,65)
    (fn k => rwp ~128 k = toIntX (~>> (i2w ~128, i2W k)));
val _ = pr_ln "test10e" test10e
val test11a = check (Word8.>  (i2w 255, i2w 254));
val _ = pr_ln "test11a" test11a
val test11b = check (Word8.<  (i2w 253, i2w 254));
val _ = pr_ln "test11b" test11b
val test11c = check (Word8.>= (i2w 128, i2w 128));
val _ = pr_ln "test11c" test11c
val test11d = check (Word8.>= (i2w 128, i2w 127));
val _ = pr_ln "test11d" test11d
val test11e = check (Word8.<= (i2w 1,   i2w 1));
val _ = pr_ln "test11e" test11e
val test11f = check (Word8.<= (i2w 0,   i2w 1));
val _ = pr_ln "test11f" test11f

val test12a = check (w2i(Word8.+(i2w   5, i2w  10)) =  15);
val _ = pr_ln "test12a" test12a
val test12b = check (w2i(Word8.+(i2w 127, i2w  11)) = 138);
val _ = pr_ln "test12b" test12b
val test12c = check (w2i(Word8.+(i2w 254, i2w   3)) =   1);
val _ = pr_ln "test12c" test12c
val test12d = check (w2i(Word8.-(i2w  10, i2w   3)) =   7);
val _ = pr_ln "test12d" test12d
val test12e = check (w2i(Word8.-(i2w 138, i2w  11)) = 127);
val _ = pr_ln "test12e" test12e
val test12f = check (w2i(Word8.-(i2w   1, i2w   3)) = 254);
val _ = pr_ln "test12f" test12f
val test12g = check (w2i(Word8.*(i2w   5, i2w  11)) =  55);
val _ = pr_ln "test12g" test12g
val test12h = check (w2i(Word8.*(i2w   4, i2w  35)) = 140);
val _ = pr_ln "test12h" test12h
val test12i = check (w2i(Word8.*(i2w   3, i2w 129)) = 131);
val _ = pr_ln "test12i" test12i
val test12j = check (w2i(Word8.div(i2w  10, i2w 3)) =   3);
val _ = pr_ln "test12j" test12j
val test12k = check (w2i(Word8.div(i2w 255, i2w 1)) = 255);
val _ = pr_ln "test12k" test12k
val test12l = check (w2i(Word8.div(i2w 242, i2w 3)) =  80);
val _ = pr_ln "test12l" test12l
val test12m = check (w2i(Word8.mod(i2w  10, i2w 3)) =   1);
val _ = pr_ln "test12m" test12m
val test12n = check (w2i(Word8.mod(i2w 255, i2w 1)) =   0);
val _ = pr_ln "test12n" test12n
val test12o = check (w2i(Word8.mod(i2w 242, i2w 3)) =   2);
val _ = pr_ln "test12o" test12o
val test12p = (Word8.div(i2w 0, i2w 256) seq "WRONG")
              handle Div => "OK" | _ => "WRONG";
val _ = pr_ln "test12p" test12p
val test12q = (Word8.mod(i2w 0, i2w 256) seq "WRONG")
              handle Div => "OK" | _ => "WRONG";
val _ = pr_ln "test12q" test12q

fun chk f (s, r) = 
    check'(fn _ => 
	   case f s of
	       SOME res => res = i2w r
	     | NONE     => false)

fun chkScan fmt = chk (StringCvt.scanString (scan fmt))

val test13a = 
    List.map (chk fromString)
             [("21", 33),
	      ("0", 0),
	      ("ff", 255),
	      ("FF", 255),
	      (" \n\t21Grap", 33),
	      ("0wx21", 33),
	      ("0wx0", 0),
	      ("0wxff", 255),
	      ("0wxFF", 255),
	      (" \n\t0wx21Grap", 33),
	      ("0x21", 33),
	      ("0x0", 0),
	      ("0xff", 255),
	      ("0XFF", 255),
	      (" \n\t0x21Grap", 33),
	      ("0wx21", 33),
	      ("0wx0", 0),
	      ("0wxff", 255),
	      ("0wxFF", 255),
	      (" \n\t0wX21Grap", 33),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0wx1", 1),
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

val test13c = (fromString "100" seq "WRONG") 
              handle Overflow => "OK" | _ => "WRONG";
val _ = pr_ln "test13c" test13c

val test13d = (fromString "FFFFFFFFFFFFFFF" seq "WRONG") 
              handle Overflow => "OK" | _ => "WRONG";
val _ = pr_ln "test13d" test13d

val test14a = 
    List.map (chkScan StringCvt.DEC)
             [("123", 123),
	      ("0", 0),
	      ("255", 255),
	      (" \n\t123crap", 123),
	      ("0w123", 123),
	      ("0w0", 0),
	      ("0w255", 255),
	      (" \n\t0w123crap", 123),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0w1", 1),
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

val test14c = (StringCvt.scanString (scan StringCvt.DEC) "256" seq "WRONG") 
              handle Overflow => "OK" | _ => "WRONG";
val _ = pr_ln "test14c" test14c

val test14d = (StringCvt.scanString (scan StringCvt.DEC) "9999999999999999" 
	       seq "WRONG") 
              handle Overflow => "OK" | _ => "WRONG";
val _ = pr_ln "test14d" test14d

val test15a = 
    List.map (chkScan StringCvt.BIN)
             [("10010", 18),
	      (" \n\t10010crap", 18),
	      ("0w10010", 18),
	      (" \n\t0w10010crap", 18),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0w1", 1),
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

val test15c = (StringCvt.scanString (scan StringCvt.BIN) "100000000" 
	       seq "WRONG") 
              handle Overflow => "OK" | _ => "WRONG";
val _ = pr_ln "test15c" test15c

val test15d = (StringCvt.scanString (scan StringCvt.BIN) 
	       "1111111111111111111111111111111111111111111111111111111111" 
	       seq "WRONG") 
              handle Overflow => "OK" | _ => "WRONG";
val _ = pr_ln "test15d" test15d

val test16a = 
    List.map (chkScan StringCvt.OCT)
             [("207", 135),
	      ("0", 0),
	      ("377", 255),
	      (" \n\t207crap", 135),
	      ("0w207", 135),
	      ("0w0", 0),
	      ("0w377", 255),
	      (" \n\t0w207crap", 135),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0w1", 1),
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

val test16c = (StringCvt.scanString (scan StringCvt.OCT) "400" seq "WRONG") 
              handle Overflow => "OK" | _ => "WRONG";
val _ = pr_ln "test16c" test16c

val test16d = (StringCvt.scanString (scan StringCvt.OCT) 
	       "7777777777777777777777777777777777777777777" seq "WRONG") 
              handle Overflow => "OK" | _ => "WRONG";
val _ = pr_ln "test16d" test16d

val test17a = 
    List.map (chkScan StringCvt.HEX)
             [("21", 33),
	      ("0", 0),
	      ("ff", 255),
	      ("FF", 255),
	      (" \n\t21Grap", 33),
	      ("0wx21", 33),
	      ("0wx0", 0),
	      ("0wxff", 255),
	      ("0wxFF", 255),
	      (" \n\t0wx21Grap", 33),
	      ("0x21", 33),
	      ("0x0", 0),
	      ("0xff", 255),
	      ("0XFF", 255),
	      (" \n\t0x21Grap", 33),
	      ("0wx21", 33),
	      ("0wx0", 0),
	      ("0wxff", 255),
	      ("0wxFF", 255),
	      (" \n\t0wX21Grap", 33),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0wx1", 1),
	      ("0w ", 0),
	      ("0wx", 0),
	      ("0wX", 0),
	      ("0wx1", 1),
	      ("0wX1", 1),
	      ("0wx ", 0),
	      ("0wX ", 0)];
val _ = pr_ln "test17a" (concat test17a)

val test17b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.HEX) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+1", "~1", "-1"];
val _ = pr_ln "test17b" (concat test17b)

val test17c = (StringCvt.scanString (scan StringCvt.HEX) "100" seq "WRONG") 
              handle Overflow => "OK" | _ => "WRONG";
val _ = pr_ln "test17c" test17c

val test17d = (StringCvt.scanString (scan StringCvt.HEX) 
	       "FFFFFFFFFFFFFFFFFF" seq "WRONG") 
              handle Overflow => "OK" | _ => "WRONG";
val _ = pr_ln "test17d" test17d

local 
    fun fromToString i = 
	fromString (toString (fromInt i)) = SOME (fromInt i);

    fun scanFmt radix i = 
	let val w = fromInt i
	    val s = fmt radix w
	in StringCvt.scanString (scan radix) s = SOME w end;

in
val test18 = 
    check'(fn _ => range (0, 255) fromToString);
val _ = pr_ln "test18" test18

val test19 = 
    check'(fn _ => range (0, 255) (scanFmt StringCvt.BIN));
val _ = pr_ln "test19" test19

val test20 = 
    check'(fn _ => range (0, 255) (scanFmt StringCvt.OCT));
val _ = pr_ln "test20" test20

val test21 = 
    check'(fn _ => range (0, 255) (scanFmt StringCvt.DEC));
val _ = pr_ln "test21" test21

val test22 = 
    check'(fn _ => range (0, 255) (scanFmt StringCvt.HEX));
val _ = pr_ln "test22" test22

end
end;

