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

(* test/widechar.sml -- test cases WideChar, suitable for ASCII
   PS 1994-12-10, 1995-05-11, 1995-11-10, 1996-09-30 *)

(*KILL 05/11/1997 10:55. tho.:
use "auxil.sml";
*)

val _ = print "File widechar.sml: Testing structure WideChar...\n"

val test4 = tstrange "test4" (0, WideChar.maxOrd) 
    (fn i => (WideChar.ord o WideChar.chr) i = i);

val test5 = tst0 "test5" ((WideChar.chr ~1 seq "WRONG") handle Chr => "OK" | _ => "WRONG")

val test6 = tst0 "test6" ((WideChar.chr (WideChar.maxOrd+1) seq "WRONG") 
                          handle Chr => "OK" | _ => "WRONG")
        
val test18 = tst "test18" (not (WideChar.contains "" (WideChar.chr 65))
                   andalso not (WideChar.contains "aBCDE" (WideChar.chr 65))
                   andalso (WideChar.contains "ABCD" (WideChar.chr 67))
                   andalso not (WideChar.contains "" #"\000")
                   andalso not (WideChar.contains "" #"\255")
                   andalso not (WideChar.contains "azAZ09" #"\000")
                   andalso not (WideChar.contains "azAZ09" #"\255"));

val test19 = tst "test19" (WideChar.notContains "" (WideChar.chr 65)
                   andalso WideChar.notContains "aBCDE" (WideChar.chr 65)
                   andalso not (WideChar.notContains "ABCD" (WideChar.chr 67))
                   andalso WideChar.notContains "" #"\000"
                   andalso WideChar.notContains "" #"\255"
                   andalso WideChar.notContains "azAZ09" #"\000"
                   andalso WideChar.notContains "azAZ09" #"\255");

val test20 = tst "test20" (WideChar.ord WideChar.maxChar = WideChar.maxOrd);

local 
fun mycontains s c = 
    let val stop = WideString.size s
        fun h i = i < stop andalso (c = WideString.sub(s, i) orelse h(i+1))
    in h 0 end;

(* Check that p(c) = (mycontains s c) for all characters: *)
fun equivalent p s = 
    let fun h n =
        n > 255 orelse 
        (p (WideChar.chr n) = mycontains s (WideChar.chr n)) andalso h(n+1)
    in h 0 end

fun checkset p s = tst' "checkset" (fn _ => equivalent p s);

val graphchars : WideString.string 
               = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\
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
    let fun h 0 res = WideChar.chr 0 :: res
          | h n res = h (n-1) (WideChar.chr n :: res)
    in h 255 [] end

open WideChar
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
    checkset isPrint (WideString.^ (" ", graphchars))
val test28 = 
    checkset isSpace " \009\010\011\012\013";
val test29 = 
    checkset isGraph graphchars
val test30 = 
    checkset isAscii ascii

val test31 = 
    tst' "test31" (fn _ => map toLower (WideString.explode ascii) = 
                           WideString.explode lowerascii)
val test32 = 
    tst' "test32" (fn _ => map toUpper (WideString.explode ascii) = 
                           WideString.explode upperascii)
val test33 = 
    tst' "test33" (fn _ => 
           map toUpper (WideString.explode graphchars)
           seq map toLower (WideString.explode graphchars)
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
    let fun chk (arg, res) = WideChar.toString arg = res
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
    let val chars = List.tabulate(256, WideChar.chr)
        fun chk c = WideChar.fromString(WideChar.toString c) = SOME c
    in tst' "test37" (fn _ => List.all chk chars) end

val test38 =                 
    let fun chkFromString (arg, res) = WideChar.fromString arg = SOME res
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
             ("\\256", #"\256"),
             ("\\999", #"\999"),
             ("\\u0000", #"\000"),
             ("\\u67ab", #"\u67ab"),
             ("\\U001067ab", #"\U001067ab"),
             ("\\U0010ffff", #"\U0010ffff"),
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
    tst' "test39" (fn _ => List.all (fn arg => WideChar.fromString arg = NONE)
           ["\\",
            "\\c",
            "\\F",
            "\\e",
            "\\g",
            "\\N",
            "\\T",
            "\\1",
            "\\11",
            "\\-65",
            "\\~65",
            "\\?",
            "\\^`",
            "\\^a",
            "\\^z",
            "\\U00110000", (* outside the range of Unicode *)
            "\\   a",
            "\\   a\\B",
            "\\   \\"]);

(* Test cases for C string escape functions *)

val test40 = 
    let val chars = List.tabulate(256, WideChar.chr)
    in tst' "test40" (fn _ => 
              List.map SOME chars 
              = List.map WideChar.fromCString (List.map WideChar.toCString chars))
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
               List.all (fn (arg, res) => WideChar.toCString arg = res) argResList)
    end;

val test42 = 
    let fun checkFromCStringSucc (arg, res) = 
            WideString.str (valOf (WideChar.fromCString arg)) = res
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
             ("\\u0000", "\000"),
             ("\\x67ab", "\u67ab"),
             ("\\u67ab", "\u67ab"),
             ("\\uffff", "\uffff"),
             ("\\U001067ab", "\U001067ab"),
             ("\\U0010ffff", "\U0010ffff"),
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
    let fun checkFromCStringFail arg = WideChar.fromCString arg = NONE
    in
        tst' "test43" (fn _ => List.all checkFromCStringFail 
               ["\\",
                "\\X",
                "\\=",
                "\\8",
                "\\9",
                "\\c",
                "\\d",
                "\\x",
                "\\U00110000", (* outside the range of Unicode *)
                "\\xG"])
    end;
