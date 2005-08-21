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


(* test/int.sml -- here we test only the `exotic' operations
   PS 1995-02-25, 1996-07-02 *)

(*KILL 05/11/1997 10:59. tho.:
use "auxil.sml";
*)

val _ = print "\nFile int.sml: Testing structure Int...\n"

local 
    open Int
    infix 7 quot rem
    fun divmod s (i, d, q, r)  = tst s (i div d = q andalso i mod d = r);
    fun quotrem s (i, d, q, r) = tst s (i quot d = q andalso i rem d = r);
in      

val test1a = divmod "test1a" (10, 3, 3, 1);
val test1b = divmod "test1b" (~10, 3, ~4, 2);
val test1c = divmod "test1c" (~10, ~3, 3, ~1);
val test1d = divmod "test1d" (10, ~3, ~4, ~2);

val test2a = quotrem "test2a" (10, 3, 3, 1);
val test2b = quotrem "test2b" (~10, 3, ~3, ~1);
val test2c = quotrem "test2c" (~10, ~3, 3, ~1);
val test2d = quotrem "test2d" (10, ~3, ~3, 1);

val test3 = tst "test3" (max(~5, 2) =  2 andalso max(5, 2) = 5);
val test4 = tst "test4" (min(~5, 3) = ~5 andalso min(5, 2) = 2);

val test5 = tst "test5" (sign ~57 = ~1 andalso sign 99 = 1 andalso sign 0 = 0);
val test6 = tst "test6" (sameSign(~255, ~256) andalso sameSign(255, 256) 
                  andalso sameSign(0, 0));

val test12 = 
    tst0 "test12" (case (minInt, maxInt) of
                     (SOME mi, SOME ma) => check(sign mi = ~1 andalso sign ma = 1 
                                                 andalso sameSign(mi, ~1) andalso sameSign(ma, 1))
                   | (NONE, NONE)       => "OK"
                   | _                  => "WRONG")

fun chk f (s, r) = 
    tst' "chk" (fn _ => 
           case f s of
               SOME res => res = r
             | NONE     => false)

fun chkScan fmt = chk (StringCvt.scanString (scan fmt))

val test13a = 
    List.map (chk fromString)
             [("10789", 10789),
              ("+10789", 10789),
              ("~10789", ~10789),
              ("-10789", ~10789),
              (" \n\t10789crap", 10789),
              (" \n\t+10789crap", 10789),
              (" \n\t~10789crap", ~10789),
              (" \n\t-10789crap", ~10789),
              ("0w123", 0),
              ("0W123", 0),
              ("0x123", 0),
              ("0X123", 0),
              ("0wx123", 0),
              ("0wX123", 0)];

val test13b = 
    List.map (fn s => tst0 "test13b" (case fromString s of NONE => "OK" | _ => "WRONG"))
           ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
            "+ 1", "~ 1", "- 1", "ff"];     

val test14a = 
    List.map (chkScan StringCvt.DEC)
             [("10789", 10789),
              ("+10789", 10789),
              ("~10789", ~10789),
              ("-10789", ~10789),
              (" \n\t10789crap", 10789),
              (" \n\t+10789crap", 10789),
              (" \n\t~10789crap", ~10789),
              (" \n\t-10789crap", ~10789),
              ("0w123", 0),
              ("0W123", 0),
              ("0x123", 0),
              ("0X123", 0),
              ("0wx123", 0),
              ("0wX123", 0)];

val test14b = 
    List.map (fn s => tst0 "test14b" (case StringCvt.scanString (scan StringCvt.DEC) s 
                      of NONE => "OK" | _ => "WRONG"))
           ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
            "+ 1", "~ 1", "- 1", "ff"];     

val test15a = 
    List.map (chkScan StringCvt.BIN)
             [("10010", 18),
              ("+10010", 18),
              ("~10010", ~18),
              ("-10010", ~18),
              (" \n\t10010crap", 18),
              (" \n\t+10010crap", 18),
              (" \n\t~10010crap", ~18),
              (" \n\t-10010crap", ~18),
              ("0w101", 0),
              ("0W101", 0),
              ("0x101", 0),
              ("0X101", 0),
              ("0wx101", 0),
              ("0wX101", 0)];

val test15b = 
    List.map (fn s => tst0 "test15b" (case StringCvt.scanString (scan StringCvt.BIN) s 
                      of NONE => "OK" | _ => "WRONG"))
           ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
            "+ 1", "~ 1", "- 1", "2", "8", "ff"];

val test16a = 
    List.map (chkScan StringCvt.OCT)
             [("2071", 1081),
              ("+2071", 1081),
              ("~2071", ~1081),
              ("-2071", ~1081),
              (" \n\t2071crap", 1081),
              (" \n\t+2071crap", 1081),
              (" \n\t~2071crap", ~1081),
              (" \n\t-2071crap", ~1081),
              ("0w123", 0),
              ("0W123", 0),
              ("0x123", 0),
              ("0X123", 0),
              ("0wx123", 0),
              ("0wX123", 0)];

val test16b = 
    List.map (fn s => tst0 "test16b" (case StringCvt.scanString (scan StringCvt.OCT) s 
                      of NONE => "OK" | _ => "WRONG"))
           ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
            "+ 1", "~ 1", "- 1", "8", "ff"];

val test17a = 
    List.map (chkScan StringCvt.HEX)
             [("20Af", 8367),
              ("+20Af", 8367),
              ("~20Af", ~8367),
              ("-20Af", ~8367),
              (" \n\t20AfGrap", 8367),
              (" \n\t+20AfGrap", 8367),
              (" \n\t~20AfGrap", ~8367),
              (" \n\t-20AfGrap", ~8367),
              ("0w123", 0),
              ("0W123", 0),
              ("0x", 0),
              ("0x ", 0),
              ("0xG", 0),
              ("0X", 0),
              ("0XG", 0),
              ("0x123", 291),
              ("0X123", 291),
              ("-0x123", ~291),
              ("-0X123", ~291),
              ("~0x123", ~291),
              ("~0X123", ~291),
              ("+0x123", 291),
              ("+0X123", 291),
              ("0wx123", 0),
              ("0wX123", 0)];

val test17b = 
    List.map (fn s => tst0 "test17b" (case StringCvt.scanString (scan StringCvt.HEX) s 
                      of NONE => "OK" | _ => "WRONG"))
           ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
            "+ 1", "~ 1", "- 1"];


local 
    fun fromToString i = 
        fromString (toString i) = SOME i;

    fun scanFmt radix i = 
        StringCvt.scanString (scan radix) (fmt radix i) = SOME i;

in
val test18 = 
    tst' "test18" (fn _ => range (~1200, 1200) fromToString);

val test19 = 
    tst' "test19" (fn _ => range (~1200, 1200) (scanFmt StringCvt.BIN));

val test20 = 
    tst' "test20" (fn _ => range (~1200, 1200) (scanFmt StringCvt.OCT));

val test21 = 
    tst' "test21" (fn _ => range (~1200, 1200) (scanFmt StringCvt.DEC));

val test22 = 
    tst' "test22" (fn _ => range (~1200, 1200) (scanFmt StringCvt.HEX));
end

end
