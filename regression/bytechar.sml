(* Auxiliary functions for test cases *)

infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") (* handle _ => "EXN" *);

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

(* test/bytechar.sml -- test cases for Byte and Char, suitable for ASCII
   PS 1994-12-10, 1995-05-11, 1995-11-10, 1996-09-30 *)

(*KILL 05/11/1997 10:55. tho.:
use "auxil.sml";
*)

val _ = print "\nFile bytechar.sml: Testing structures Byte and Char...\n"

local 

in 

val test1 = tstrange "test1" (0,255) (fn i => 
    (Word8.toInt o Byte.charToByte o Byte.byteToChar o Word8.fromInt) i = i);

val test2 = tstrange "test2" (0,Char.maxOrd) (fn i => 
    (Word8.toInt o Byte.charToByte o Char.chr) i = i);

val test3 = tstrange "test3" (0,255) 
    (fn i => (Char.ord o Byte.byteToChar o Word8.fromInt) i = i);

val test4 = tstrange "test4" (0, Char.maxOrd) 
    (fn i => (Char.ord o Char.chr) i = i);

val test5 = tst0 "test5" ((Char.chr ~1 seq "WRONG") handle Chr => "OK" | _ => "WRONG")

val test6 = tst0 "test6" ((Char.chr (Char.maxOrd+1) seq "WRONG") 
                          handle Chr => "OK" | _ => "WRONG")
        
val test7 = tst "test7" ("" = Byte.bytesToString (Word8Vector.fromList []));

val test8 = 
    tst "test8" ("ABDC" = 
          (Byte.bytesToString o Word8Vector.fromList o map Word8.fromInt)
           [65, 66, 68, 67]);

val unpackString = Byte.unpackString o Word8ArraySlice.slice
   
val test9 = tst "test9" ("" = unpackString (Word8Array.fromList [], 0, SOME 0))

local 
    val arr = Word8Array.tabulate(10, fn i => Word8.fromInt(i+65))
in
val test10a = tst "test10a" ("" = unpackString(arr, 0, SOME 0));
val test10b = tst "test10b" ("" = unpackString(arr, 10, SOME 0) 
                             andalso "" = unpackString(arr, 10, NONE));
val test10c = tst "test10c" ("BCDE" = unpackString(arr, 1, SOME 4));
val test10d = tst0 "test10d" ((unpackString(arr, ~1, SOME 0) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
val test10e = tst0 "test10e" ((unpackString(arr, 11, SOME 0) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
val test10f = tst0 "test10f" ((unpackString(arr, 0, SOME ~1) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
val test10g = tst0 "test10g" ((unpackString(arr, 0, SOME 11) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
val test10h = tst0 "test10h" ((unpackString(arr, 10, SOME 1) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
val test10i = tst0 "test10i" ((unpackString(arr, ~1, NONE) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
val test10j = tst0 "test10j" ((unpackString(arr, 11, NONE) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
end

val unpackStringVec = Byte.unpackStringVec o Word8VectorSlice.slice
local 
    val vec = Word8Vector.tabulate(10, fn i => Word8.fromInt(i+65))
in
val test11a = tst "test11a" ("" = unpackStringVec(vec, 0, SOME 0));
val test11b = tst "test11b" ("" = unpackStringVec(vec, 10, SOME 0) 
                             andalso "" = unpackStringVec(vec, 10, NONE));
val test11c = tst "test11c" ("BCDE" = unpackStringVec(vec, 1, SOME 4));
val test11d = tst0 "test11d" ((unpackStringVec(vec, ~1, SOME 0) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
val test11e = tst0 "test11e" ((unpackStringVec(vec, 11, SOME 0) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
val test11f = tst0 "test11f" ((unpackStringVec(vec, 0, SOME ~1) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
val test11g = tst0 "test11g" ((unpackStringVec(vec, 0, SOME 11) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
val test11h = tst0 "test11h" ((unpackStringVec(vec, 10, SOME 1) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
val test11i = tst0 "test11i" ((unpackStringVec(vec, ~1, NONE) seq "WRONG")
                            handle Subscript => "OK" | _ => "WRONG")
val test11j = tst0 "test11j" ((unpackStringVec(vec, 11, NONE) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
end

val test18 = tst "test18" (not (Char.contains "" (Char.chr 65))
                   andalso not (Char.contains "aBCDE" (Char.chr 65))
                   andalso (Char.contains "ABCD" (Char.chr 67))
                   andalso not (Char.contains "" #"\000")
                   andalso not (Char.contains "" #"\255")
                   andalso not (Char.contains "azAZ09" #"\000")
                   andalso not (Char.contains "azAZ09" #"\255"));

val test19 = tst "test19" (Char.notContains "" (Char.chr 65)
                   andalso Char.notContains "aBCDE" (Char.chr 65)
                   andalso not (Char.notContains "ABCD" (Char.chr 67))
                   andalso Char.notContains "" #"\000"
                   andalso Char.notContains "" #"\255"
                   andalso Char.notContains "azAZ09" #"\000"
                   andalso Char.notContains "azAZ09" #"\255");

val test20 = tst "test20" (Char.ord Char.maxChar = Char.maxOrd);

local 
fun mycontains s c = 
    let val stop = String.size s
        fun h i = i < stop andalso (c = String.sub(s, i) orelse h(i+1))
    in h 0 end;

(* Check that p(c) = (mycontains s c) for all characters: *)
fun equivalent p s = 
    let fun h n =
        n > 255 orelse 
        (p (chr n) = mycontains s (chr n)) andalso h(n+1)
    in h 0 end

fun checkset p s = tst' "checkset" (fn _ => equivalent p s);

val graphchars = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\
 \[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";

val ascii = "\^@\^A\^B\^C\^D\^E\^F\^G\^H\t\n\^K\^L\^M\^N\^O\^P\
 \\^Q\^R\^S\^T\^U\^V\^W\^X\^Y\^Z\^[\^\\^]\^^\^_\
 \ !\"#$%&'()*+,-./0123456789:;<=>?@\
 \ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127" 

val lowerascii = "\^@\^A\^B\^C\^D\^E\^F\^G\^H\t\n\^K\^L\^M\^N\^O\^P\
 \\^Q\^R\^S\^T\^U\^V\^W\^X\^Y\^Z\^[\^\\^]\^^\^_\
 \ !\"#$%&'()*+,-./0123456789:;<=>?@\
 \abcdefghijklmnopqrstuvwxyz[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127" 

val upperascii = "\^@\^A\^B\^C\^D\^E\^F\^G\^H\t\n\^K\^L\^M\^N\^O\^P\
 \\^Q\^R\^S\^T\^U\^V\^W\^X\^Y\^Z\^[\^\\^]\^^\^_\
 \ !\"#$%&'()*+,-./0123456789:;<=>?@\
 \ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~\127" 

val allchars = 
    let fun h 0 res = chr 0 :: res
          | h n res = h (n-1) (chr n :: res)
    in h 255 [] end

open Char
in

val test21 = 
    checkset isLower "abcdefghijklmnopqrstuvwxyz";
val test22 = 
    checkset isUpper "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
val test23 = 
    checkset isDigit "0123456789";
val test24 = 
    checkset isAlpha "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
val test25 = 
    checkset isHexDigit "0123456789abcdefABCDEF";
val test26 = 
    checkset isAlphaNum 
       "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
val test27 = 
    checkset isPrint (" " ^ graphchars)
val test28 = 
    checkset isSpace " \009\010\011\012\013";
val test29 = 
    checkset isGraph graphchars
val test30 = 
    checkset isAscii ascii

val test31 = 
    tst' "test31" (fn _ => map toLower (explode ascii) = explode lowerascii)
val test32 = 
    tst' "test32" (fn _ => map toUpper (explode ascii) = explode upperascii)
val test33 = 
    tst' "test33" (fn _ => 
           map toUpper (explode graphchars)
           seq map toLower (explode graphchars)
           seq true)

val test34a =
    tst' "test34a" (fn _ => 
           map pred (List.drop(allchars, 1)) = List.take(allchars, 255));
val test34b = tst0 "test34b" ((pred minChar seq "WRONG")
                              handle Chr => "OK" | _ => "WRONG")
val test35a =
    tst' "test35a" (fn _ => 
           map succ (List.take(allchars, 255)) = List.drop(allchars, 1));
val test35b = tst0 "test35b" ((succ maxChar seq "WRONG")
                              handle Chr => "OK" | _ => "WRONG")
end


(* Test cases for SML character escape functions. *)

val test36 = 
    let fun chk (arg, res) = Char.toString arg = res
    in tst' "test36" (fn _ => List.all chk 
              [(#"\000", "\\^@"),
               (#"\001", "\\^A"),
               (#"\006", "\\^F"),
               (#"\007", "\\a"),
               (#"\008", "\\b"),
               (#"\009", "\\t"),
               (#"\010", "\\n"),
               (#"\011", "\\v"),
               (#"\012", "\\f"),
               (#"\013", "\\r"),
               (#"\014", "\\^N"),
               (#"\031", "\\^_"),
               (#"\032", " "),
               (#"\126", "~"),
               (#"\\", "\\\\"),
               (#"\"", "\\\""),
               (#"A", "A"),
               (#"\127", "\\127"),
               (#"\128", "\\128"),
               (#"\255", "\\255")])
    end;

val test37 = 
    let val chars = List.tabulate(256, chr)
        fun chk c = Char.fromString(Char.toString c) = SOME c
    in tst' "test37" (fn _ => List.all chk chars) end

val test38 =                 
    let fun chkFromString (arg, res) = Char.fromString arg = SOME res
        val argResList = 
            [("A", #"A"),
             ("z", #"z"),
             ("@", #"@"),
             ("~", #"~"),
             ("\\a", #"\007"),
             ("\\b", #"\008"),
             ("\\t", #"\009"),
             ("\\n", #"\010"),
             ("\\v", #"\011"),
             ("\\f", #"\012"),
             ("\\r", #"\013"),
             ("\\\\", #"\\"),
             ("\\\"", #"\""),
             ("\\^@", #"\000"),
             ("\\^A", #"\001"),
             ("\\^Z", #"\026"),
             ("\\^_", #"\031"), 
             ("\\000", #"\000"),
             ("\\097", #"a"),
             ("\\255", #"\255"),
             ("\\   \t\n\n \\A", #"A"),
             ("\\   \t\n\n \\z", #"z"),
             ("\\   \t\n\n \\@", #"@"),
             ("\\   \t\n\n \\~", #"~"),
             ("\\   \t\n\n \\\\n", #"\n"),
             ("\\   \t\n\n \\\\t", #"\t"),
             ("\\   \t\n\n \\\\\\", #"\\"),
             ("\\   \t\n\n \\\\\"", #"\""),
             ("\\   \t\n\n \\\\^@", #"\000"),
             ("\\   \t\n\n \\\\^A", #"\001"),
             ("\\   \t\n\n \\\\^Z", #"\026"),
             ("\\   \t\n\n \\\\^_", #"\031"), 
             ("\\   \t\n\n \\\\000", #"\000"),
             ("\\   \t\n\n \\\\097", #"a"),
             ("\\   \t\n\n \\\\255", #"\255")]
    in 
        tst' "test38" (fn _ => List.all chkFromString argResList)
    end;

val test39 = 
    tst' "test39" (fn _ => List.all (fn arg => Char.fromString arg = NONE)
           ["\\",
            "\\c",
            "\\F",
            "\\e",
            "\\g",
            "\\N",
            "\\T",
            "\\1",
            "\\11",
            "\\256",
            "\\-65",
            "\\~65",
            "\\?",
            "\\^`",
            "\\^a",
            "\\^z",
            "\\   a",
            "\\   a\\B",
            "\\   \\"]);

(* Test cases for C string escape functions *)

val test40 = 
    let val chars = List.tabulate(256, chr)
    in tst' "test40" (fn _ => 
              List.map SOME chars 
              = List.map Char.fromCString (List.map Char.toCString chars))
    end;

val test41 = 
    let val argResList = 
            [(#"\010", "\\n"),
             (#"\009", "\\t"),
             (#"\011", "\\v"),
             (#"\008", "\\b"),
             (#"\013", "\\r"),
             (#"\012", "\\f"),
             (#"\007", "\\a"),
             (#"\\", "\\\\"),
             (#"?", "\\?"),
             (#"'", "\\'"),
             (#"\"", "\\\"")]
    in
        tst' "test41" (fn _ => 
               List.all (fn (arg, res) => Char.toCString arg = res) argResList)
    end;

val test42 = 
    let fun checkFromCStringSucc (arg, res) = 
            str (valOf (Char.fromCString arg)) = res
        val argResList = 
            [("\\n", "\010"),
             ("\\t", "\009"),
             ("\\v", "\011"),
             ("\\b", "\008"),
             ("\\r", "\013"),
             ("\\f", "\012"),
             ("\\a", "\007"),
             ("\\\\",  "\\"),
             ("\\?", "?"),
             ("\\'", "'"),
             ("\\\"", "\""),
             ("\\1", "\001"),
             ("\\11", "\009"),
             ("\\111", "\073"),
             ("\\1007", "\064"),
             ("\\100A", "\064"),
             ("\\0",   "\000"),
             ("\\377", "\255"),
             ("\\18", "\001"),
             ("\\178", "\015"),
             ("\\1C", "\001"),
             ("\\17C", "\015"),
             ("\\x0", "\000"),
             ("\\xff", "\255"),
             ("\\xFF", "\255"),
             ("\\x1", "\001"),
             ("\\x11", "\017"),
             ("\\xag", "\010"),
             ("\\xAAg", "\170"),
             ("\\x0000000a", "\010"),
             ("\\x0000000a2", "\162"),
             ("\\x0000000ag", "\010"),
             ("\\x0000000A", "\010"),
             ("\\x0000000A2", "\162"),
             ("\\x0000000Ag", "\010"),
             ("\\x00000000000000000000000000000000000000000000000000000000000000011+",
              "\017")
             ]
    in 
        tst' "test42" (fn _ => List.all checkFromCStringSucc argResList)
    end;

val test43 = 
    let fun checkFromCStringFail arg = Char.fromCString arg = NONE
    in
        tst' "test43" (fn _ => List.all checkFromCStringFail 
               ["\\",
                "\\X",
                "\\=",
                "\\400",
                "\\777",
                "\\8",
                "\\9",
                "\\c",
                "\\d",
                "\\x",
                "\\x100",
                "\\xG"])
    end;
end
