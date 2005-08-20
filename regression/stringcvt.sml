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


(* test/stringcvt.sml -- 1995-10-26, 1996-07-05 *)

(*KILL 05/11/1997 11:02. tho.:
use "auxil.sml";
*)

val _ = print "\nFile stringcvt.sml: Testing structure StringCvt...\n";

local 
    open StringCvt
    (* Read all upper case letters, skip lowercase letters, scan an
     * integer, and return the excess characters: *)

    fun triple getc src =
        let open StringCvt
            val (str1, src1) = splitl Char.isUpper getc src
            val src2         = dropl Char.isLower getc src1
        in case Int.scan DEC getc src2 of
            NONE            => NONE
          | SOME (i, src3)  => 
                let val str2 = takel (fn _ => true) getc src3
                in SOME((str1, i, str2), src3) end
        end

    (* Testing TextIO.scanStream: *)

    val tmpfile = "textio.tmp";
    fun putandscan scan s = 
        let open TextIO 
            val os = openOut tmpfile
            val _  = output(os, s)
            val _  = closeOut os 
            val is = openIn tmpfile
        in
            scanStream scan is
            before 
            closeIn is
        end;
            
    fun testtrip (s, res) = 
        scanString triple s = res
        andalso putandscan triple s = res

    datatype result = Bool of bool | Int of int

    fun backtrack getc src = 
        case Bool.scan getc src of
            SOME(b, rest) => SOME (Bool b, rest)
          | NONE          => 
                case Int.scan StringCvt.DEC getc src of
                    SOME(i, rest) => SOME(Int i, rest)
                  | NONE   => 
                        case Int.scan StringCvt.HEX getc src of
                            SOME(i, rest) => SOME(Int i, rest)
                          | NONE   => NONE

    fun testback (s, res) =
        scanString backtrack s = res
        andalso putandscan backtrack s = res

in

val test1 = 
    tst' "test1" (fn _ =>
           padLeft #"#" 0 "abcdef" = "abcdef"
           andalso padLeft #"#" 6 "abcdef" = "abcdef"
           andalso padLeft #"#" 7 "abcdef" = "#abcdef"
           andalso padLeft #"#" 10 "abcdef" = "####abcdef"
           andalso padLeft #"#" ~3 "abcdef" = "abcdef");

val test2 = 
    tst' "test2" (fn _ =>
           padRight #"#" 0 "abcdef" = "abcdef"
           andalso padRight #"#" 6 "abcdef" = "abcdef"
           andalso padRight #"#" 7 "abcdef" = "abcdef#"
           andalso padRight #"#" 10 "abcdef" = "abcdef####"
           andalso padRight #"#" ~3 "abcdef" = "abcdef");

val test3 = 
    tst' "test3" (fn _ =>
    testtrip ("", NONE)
    andalso testtrip(" a1", NONE)
    andalso testtrip(" A1", NONE)
    andalso testtrip("ABC A1", NONE)
    andalso testtrip("ABC a1", NONE)
    andalso testtrip(" *1", NONE)
    andalso testtrip("ABC *1", NONE));

val test4 = 
    tst' "test4" (fn _ =>
    testtrip ("1", SOME("", 1, ""))
    andalso testtrip ("1", SOME("", 1, ""))
    andalso testtrip (" 1", SOME("", 1, ""))
    andalso testtrip (" 1  ", SOME("", 1, "  ")));

val test5 =
    tst' "test5" (fn _ =>
    testtrip ("1a123+ +&D", SOME("", 1, "a123+ +&D"))
    andalso testtrip ("1a123+ +&D", SOME("", 1, "a123+ +&D"))
    andalso testtrip ("a1a123+ +&D", SOME("", 1, "a123+ +&D"))
    andalso testtrip ("a1a123+ +&D", SOME("", 1, "a123+ +&D"))
    andalso testtrip ("azbc1a123+ +&D", SOME("", 1, "a123+ +&D"))
    andalso testtrip ("azbc1a123+ +&D", SOME("", 1, "a123+ +&D"))
    andalso testtrip ("azbc  1a123+ +&D", SOME("", 1, "a123+ +&D"))
    andalso testtrip ("azbc  1a123+ +&D", SOME("", 1, "a123+ +&D")))

val test6 = 
    tst' "test6" (fn _ =>
    testtrip ("~1234a123+ +&D", SOME("", ~1234, "a123+ +&D"))
    andalso testtrip ("~1234a123+ +&D", SOME("", ~1234, "a123+ +&D"))
    andalso testtrip ("a~1234a123+ +&D", SOME("", ~1234, "a123+ +&D"))
    andalso testtrip ("a~1234a123+ +&D", SOME("", ~1234, "a123+ +&D"))
    andalso testtrip ("azbc~1234a123+ +&D", SOME("", ~1234, "a123+ +&D"))
    andalso testtrip ("azbc~1234a123+ +&D", SOME("", ~1234, "a123+ +&D"))
    andalso testtrip ("azbc  ~1234a123+ +&D", SOME("", ~1234, "a123+ +&D"))
    andalso testtrip ("azbc  ~1234a123+ +&D", SOME("", ~1234, "a123+ +&D")))

val test7 =
    tst' "test7" (fn _ =>
    testtrip ("A1a123+ +&D", SOME("A", 1, "a123+ +&D"))
    andalso testtrip ("ABCDEFG1a123+ +&D", SOME("ABCDEFG", 1, "a123+ +&D"))
    andalso testtrip ("Aa1a123+ +&D", SOME("A", 1, "a123+ +&D"))
    andalso testtrip ("ABCDEFGa1a123+ +&D", SOME("ABCDEFG", 1, "a123+ +&D"))
    andalso testtrip ("Aazbc1a123+ +&D", SOME("A", 1, "a123+ +&D"))
    andalso testtrip ("ABCDEFGazbc1a123+ +&D", SOME("ABCDEFG", 1, "a123+ +&D"))
    andalso testtrip ("Aazbc  1a123+ +&D", SOME("A", 1, "a123+ +&D"))
    andalso testtrip ("ABCDEFGazbc  1a123+ +&D", SOME("ABCDEFG", 1, "a123+ +&D")))

val test8 = 
    tst' "test8" (fn _ =>
    testtrip ("A~1234a123+ +&D", SOME("A", ~1234, "a123+ +&D"))
    andalso 
    testtrip ("ABCDEFG~1234a123+ +&D", SOME("ABCDEFG", ~1234, "a123+ +&D"))
    andalso testtrip ("Aa~1234a123+ +&D", SOME("A", ~1234, "a123+ +&D"))
    andalso 
    testtrip ("ABCDEFGa~1234a123+ +&D", SOME("ABCDEFG", ~1234, "a123+ +&D"))
    andalso testtrip ("Aazbc~1234a123+ +&D", SOME("A", ~1234, "a123+ +&D"))
    andalso 
    testtrip ("ABCDEFGazbc~1234a123+ +&D", SOME("ABCDEFG", ~1234, "a123+ +&D"))
    andalso testtrip ("Aazbc  ~1234a123+ +&D", SOME("A", ~1234, "a123+ +&D"))
    andalso 
    testtrip ("ABCDEFGazbc  ~1234a123+ +&D", SOME("ABCDEFG", ~1234, "a123+ +&D")))

val test9 = 
    tst' "test9" (fn _ =>
    let fun getstring b getc src = 
            SOME(takel (fn _ => b) getc src, src)
        fun dup 0 s = s
          | dup n s = dup (n-1) (s^s);
        val longstring = dup 13 "abcDEFGHI"
    in 
        scanString (getstring true) longstring = SOME longstring 
        andalso scanString (getstring false) longstring = SOME ""
        andalso putandscan (getstring true) longstring = SOME longstring
    end)

val test10 = 
    tst' "test10" (fn _ => 
           List.all testback
           [("false",  SOME (Bool false)),
            ("true",   SOME (Bool true)),
            ("tru e",  NONE),
            ("fals e", SOME (Int 250)),
            ("fa",     SOME (Int 250)),
            ("fa00",   SOME (Int 64000)),
            ("21a",    SOME (Int 21)),
            ("a21",    SOME (Int 2593)),
            ("",       NONE),
            ("gryf",   NONE)
            ]);
    
(*val _ = FileSys.remove tmpfile*)

end
