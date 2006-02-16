(* test/substring.sml 1995-04-27, 1997-06-03 *)

infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int 
    in
        (from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");
fun tst  s b = tst0 s (check  b);
fun tst' s f = tst0 s (check' f);

fun checkrange bounds = check o range bounds;


local 
    open Char Substring
    val all = full
    fun base2 (a, b) = (base a, base b)

    val s1 = ""                         (* String.size s1 =  0 *)
    and s2 = "ABCDE\tFGHI";             (* String.size s2 = 10 *)
    val ss1 = all s1                    (* size s1 =  0 *)
    and ss2 = all s2;                   (* size s2 = 10 *)

    val sa = "AAAAaAbAABBBB";           (* String.size sa = 14 *)
    (*            45678      *)

    val ssa1 = extract(sa, 4, SOME 0)   (* size ssa1 = 0 *)
    val ssa2 = extract(sa, 4, SOME 5)   (* size ssa2 = 5 *)

    val ss3 = extract("junk this is a   (clear)textjunk", 4, SOME 24);
    (*                       456789012345678901234567        *)

in


val test1a = tst' "test1a"
    (fn _ => 
           (s2, 10, 0) = base(extract(s2, 10, SOME 0))
           andalso (s2, 10, 0) = base(extract(s2, 10, NONE))
           andalso (s2, 0,  0) = base(extract(s2, 0, SOME 0))
           andalso (s2, 4,  3) = base(extract(s2, 4, SOME 3))
           andalso (s2, 4,  6) = base(extract(s2, 4, SOME 6))
           andalso (s2, 4,  6) = base(extract(s2, 4, NONE))
           andalso (s2, 0, 10) = base(extract(s2, 0, SOME 10))
           andalso (s2, 0, 10) = base(extract(s2, 0, NONE)));

val test1b = tst0 "test1b" ((extract(s2, ~1, SOME 0) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test1c = tst0 "test1c" ((extract(s2, 11, SOME 0) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test1d = tst0 "test1d" ((extract(s2, 0, SOME 11) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test1e = tst0 "test1e" ((extract(s2, 10, SOME 1) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test1f = tst0 "test1f" ((extract(s2, ~1, NONE) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test1g = tst0 "test1g" ((extract(s2, 11, NONE) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")

val test1h = tst' "test1h"
    (fn _ =>
           string ssa1 = ""
           andalso string ssa2 = "aAbAA"
           andalso s1 = string (all s1) 
           andalso s2 = string (all s2));

val test2a = tst' "test2a" 
    (fn _ => 
           string(triml 6 ss2) = "FGHI"
           andalso s2 = string(triml 0 ss2)
           andalso s1 = string(triml 0 ss1)
           andalso (s2, 10, 0) = base(triml 10 ss2)
           andalso (s2, 10, 0) = base(triml 11 ss2)
           andalso (sa, 6, 3) = base(triml 2 ssa2)
           andalso (sa, 9, 0) = base(triml 5 ssa2)
           andalso (sa, 9, 0) = base(triml 6 ssa2));

val test2b = tst0 "test2b" ((triml ~1 ss2 seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test2c = tst0 "test2c" ((triml ~1 ssa2 seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")

val test3a = tst' "test3a" 
    (fn _ => 
           string(trimr 6 ss2) = "ABCD"
           andalso s2 = string(trimr 0 ss2)
           andalso s1 = string(trimr 0 ss1)
           andalso (s2, 0, 0) = base(trimr 10 ss2)
           andalso (s2, 0, 0) = base(trimr 11 ss2)
           andalso (sa, 4, 3) = base(trimr 2 ssa2)
           andalso (sa, 4, 0) = base(trimr 5 ssa2)
           andalso (sa, 4, 0) = base(trimr 6 ssa2));

val test3b = tst0 "test3b" ((trimr ~1 ss2 seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test3c = tst0 "test3c" ((trimr ~1 ssa2 seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")

val test4 = tst' "test4" 
    (fn _ => 
           isEmpty ss1 
           andalso not (isEmpty ss2)
           andalso isEmpty ssa1
           andalso not (isEmpty ssa2));

val test5a = tst' "test5a" 
    (fn _ =>
           case getc ssa1 of NONE => true | _ => false);

val test5b = tst' "test5b" 
    (fn _ =>
           case getc ssa2 of
               NONE             => false 
             | SOME(#"a", rest) => "AbAA" = string rest
             | _                => false);

val test6 = tst' "test6" 
    (fn _ =>
           first ssa1 = NONE
           andalso first ssa2 = SOME #"a")

val test7 = tst' "test7" 
    (fn _ => (size ss1 = 0 andalso size ss2 = 10
                    andalso size ssa1 = 0 andalso size ssa2 = 5));

val test8a = tst' "test8a" 
    (fn _ => (sub(ss2,6) = chr 70 andalso sub(ss2,9) = chr 73
                    andalso sub(ssa2, 1) = chr 65));
val test8b = tst0 "test8b" 
   ((sub(ss1, 0)  seq "WRONG") handle Subscript => "OK" | _ => "WRONG")
val test8c = tst0 "test8c" 
   ((sub(ss2, ~1) seq "WRONG") handle Subscript => "OK" | _ => "WRONG")
val test8d = tst0 "test8d" 
   ((sub(ss2, 10) seq "WRONG") handle Subscript => "OK" | _ => "WRONG")
val test8e = tst0 "test8e" 
   ((sub(ssa2, ~1) seq "WRONG") handle Subscript => "OK" | _ => "WRONG")
val test8f = tst0 "test8f" 
   ((sub(ssa2, 5) seq "WRONG") handle Subscript => "OK" | _ => "WRONG")

val test9a = tst' "test9a" 
    (fn _ => 
           base ss2 = base(slice(ss2, 0, SOME (size ss2)))
           andalso base ss2 = base(slice(ss2, 0, NONE))
           andalso (s2, 10, 0) = base(slice(ss2, size ss2, SOME 0))
           andalso (s2, 10, 0) = base(slice(ss2, size ss2, NONE))
           andalso base ss1 = base(slice(ss1, 0, SOME 0))
           andalso base ss1 = base(slice(ss1, 0, NONE)));

val test9b = tst' "test9b" 
    (fn _ => 
                   (sa, 4, 5) = base(slice(ssa2, 0, SOME 5))
           andalso (sa, 4, 5) = base(slice(ssa2, 0, NONE))
           andalso (sa, 4, 0) = base(slice(ssa2, 0, SOME 0))
           andalso (sa, 9, 0) = base(slice(ssa2, 5, SOME 0))
           andalso (sa, 9, 0) = base(slice(ssa2, 5, NONE))
           andalso (sa, 5, 3) = base(slice(ssa2, 1, SOME 3))
           andalso (sa, 5, 4) = base(slice(ssa2, 1, SOME 4))
           andalso (sa, 5, 4) = base(slice(ssa2, 1, NONE)));

val test9c = tst0 "test9c" ((slice(ssa2, ~1, SOME 0) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9d = tst0 "test9d" ((slice(ssa2, 6, SOME 0) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9e = tst0 "test9e" ((slice(ssa2, 0, SOME 6) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9f = tst0 "test9f" ((slice(ssa2, 5, SOME 1) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9g = tst0 "test9g" ((slice(ssa2, ~1, NONE) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")
val test9h = tst0 "test9h" ((slice(ssa2, 6, NONE) seq "WRONG") 
                            handle Subscript => "OK" | _ => "WRONG")

val test12 = tst' "test12"
    (fn _ => 
           concat [] = ""
           andalso concat [ssa1, ssa1, ssa1] = ""
           andalso concat [ssa2, ssa2, ssa2] = "aAbAAaAbAAaAbAA"
           andalso concat [ssa2, ssa1, ss2, ss1] = "aAbAAABCDE\tFGHI");

val test13 = tst' "test13" 
    (fn _ => 
           explode ss1 = []
           andalso explode ssa1 = []
           andalso explode ssa2 = [#"a", #"A", #"b", #"A", #"A"]);

val test14 = tst' "test14" 
    (fn _ => 
           EQUAL = compare(ssa1,ssa1) andalso EQUAL = compare(ssa2,ssa2)
           andalso LESS = compare(triml 1 ssa2, ssa2)
           andalso GREATER = compare(ssa2, triml 1 ssa2)
           andalso LESS = compare(trimr 1 ssa2, ssa2)
           andalso GREATER = compare(ssa2, trimr 1 ssa2)
           andalso LESS = compare(all "AB", ssa2)
           andalso GREATER = compare(ssa2, all "AB"));

fun finda c = c <> #"A";
fun findb c = c <> #"B";

val test15 = tst' "test15" 
    (fn _ =>
           (sa, 5, 4) = base(dropl finda ssa2)
           andalso (sa, 9, 0) = base(dropl findb ssa2)
           andalso base ssa1 = base(dropl finda ssa1));

val test16 = tst' "test16" 
    (fn _ =>
           (sa, 4, 5) = base(dropr finda ssa2)
           andalso (sa, 4, 0) = base(dropr findb ssa2)
           andalso base ssa1 = base(dropr finda ssa1));

val test17 = tst' "test17" 
    (fn _ =>
           (sa, 4, 1) = base(takel finda ssa2)
           andalso (sa, 4, 5) = base(takel findb ssa2)
           andalso base ssa1 = base(takel finda ssa1));
    
val test18 = tst' "test18" 
    (fn _ =>
           (sa, 9, 0) = base(taker finda ssa2)
           andalso (sa, 4, 5) = base(taker findb ssa2)
           andalso base ssa1 = base(taker finda ssa1));

val test19 = tst' "test19"
    (fn _ => 
           ((sa, 4, 1), (sa, 5, 4)) = base2(splitl finda ssa2)
           andalso ((sa, 4, 5), (sa, 9, 0)) = base2(splitl findb ssa2)
           andalso base2(ssa1, ssa1) = base2(splitl finda ssa1));

val test20 = tst' "test20"
    (fn _ => 
           ((sa, 4, 5), (sa, 9, 0)) = base2(splitr finda ssa2)
           andalso ((sa, 4, 0), (sa, 4, 5)) = base2(splitr findb ssa2)
           andalso base2(ssa1, ssa1) = base2 (splitr finda ssa1));

val test21 = tst' "test21" 
    (fn _ => 
           ((sa, 4, 0), (sa, 4, 5)) = base2(position "" ssa2)
           andalso ((sa, 4, 1), (sa, 5, 4)) = base2(position "Ab" ssa2)
           andalso ((sa, 4, 5), (sa, 9, 0)) = base2(position "B" ssa2)
           andalso ((sa, 4, 5), (sa, 9, 0)) = base2(position "AAB" ssa2)
           andalso ((sa, 4, 0), (sa, 4, 5)) = base2(position "aA" ssa2)
           andalso ((sa, 4, 2), (sa, 6, 3)) = base2(position "bAA" ssa2)
           andalso (base ssa1, base ssa1) = base2(position "A" ssa1)
           andalso (base ssa1, base ssa1) = base2(position "" ssa1));

(* For the pre-November 1995 version of position: 
val test21 = tst' "test21" 
    (fn _ => 
           (sa, 4, 5) = base(position "" ssa2)
           andalso (sa, 5, 4) = base(position "Ab" ssa2)
           andalso (sa, 9, 0) = base(position "B" ssa2)
           andalso (sa, 9, 0) = base(position "AAB" ssa2)
           andalso (sa, 4, 5) = base(position "aA" ssa2)
           andalso (sa, 6, 3) = base(position "bAA" ssa2)
           andalso base ssa1 = base(position "A" ssa1)
           andalso base ssa1 = base(position "" ssa1));
*)

val test22a = tst' "test22a" 
    (fn _ => 
           (translate (fn _ => "") ssa2 = ""
            andalso translate (fn x => str x) ssa1 = ""
            andalso translate (fn x => str x) ssa2 = string ssa2));

val test22b = tst' "test22b" 
    (fn _ => 
           (translate (fn c => if c = #"b" then "XYZ " else str c) ssa2
                      = "aAXYZ AA"));

val test23 = tst' "test23" 
    (fn _ => 
           (null(tokens isSpace ssa1)
            andalso null(tokens (Char.contains "Aab") ssa2)
            andalso map string (tokens (fn c => c = #"A") ssa2) = ["a","b"]));

val test24 = tst' "test24" 
    (fn _ => 
           (map base (fields isSpace ssa1) = [base ssa1]
            andalso map base (fields (contains "Aab") ssa2)
                    = [(sa,4,0),(sa,5,0),(sa,6,0),(sa,7,0),(sa,8,0),(sa,9,0)]
            andalso map string (fields (fn c => c = #"A") ssa2) 
                    = ["a","b","",""]));

val test25 = tst' "test25" 
    (fn _ => 
           null(tokens (fn _ => true) ss3)
           andalso null(tokens (fn _ => false) (all ""))
           andalso null(tokens (contains " ()") (all "(()())(( ()"))
           andalso ["this","is","a","clear","text"] = 
                           map string (tokens (contains " ()") ss3));

local 
    val v = ref 0
    fun setv c = v := ord c;
in 
    
val test26a = tst' "test26a" 
    (fn _ => 
           (v := 0;
            foldl (fn (x, _) => setv x) () ssa2;
            !v = 65));

val test26b = tst' "test26b" 
    (fn _ => 
           implode(foldl (op ::) [] ssa2) = "AAbAa");

val test27a = tst' "test27a" 
    (fn _ => 
           (v := 0;
           foldr (fn (x, _) => setv x) () ssa2;
           !v = 97));

val test27b = tst' "test27b" 
    (fn _ => 
           implode(foldr (op ::) [] ssa2) = "aAbAA");

val test28 = tst' "test28" 
    (fn _ => 
           (v := 0;
            app setv ssa2;
            !v = 65));
end

val test29a = tst' "test29a" 
    (fn _ =>
           base2(splitAt(ssa1, 0)) = ((sa, 4, 0), (sa, 4, 0))
           andalso base2(splitAt(ssa2, 0)) = ((sa, 4, 0), (sa, 4, 5))
           andalso base2(splitAt(ssa2, 1)) = ((sa, 4, 1), (sa, 5, 4))
           andalso base2(splitAt(ssa2, 4)) = ((sa, 4, 4), (sa, 8, 1))
           andalso base2(splitAt(ssa2, 5)) = ((sa, 4, 5), (sa, 9, 0)));
val test29b = tst0 "test29b" ((splitAt(ssa2, ~1) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")
val test29c = tst0 "test29c" ((splitAt(ssa2, 6) seq "WRONG")
                              handle Subscript => "OK" | _ => "WRONG")

val test30a = tst' "test30a" 
    (fn _ => 
           (s2, 10, 0) = base(substring(s2, 10, 0))
           andalso (s2, 0,  0) = base(substring(s2, 0, 0))
           andalso (s2, 4,  3) = base(substring(s2, 4, 3))
           andalso (s2, 4,  6) = base(substring(s2, 4, 6))
           andalso (s2, 0, 10) = base(substring(s2, 0, 10)));

val test30b = tst0 "test30b" ((substring(s2, ~1, 0) seq "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")
val test30c = tst0 "test30c" ((substring(s2, 11, 0) seq "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")
val test30d = tst0 "test30d" ((substring(s2, 0, 11) seq "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")
val test30e = tst0 "test30e" ((substring(s2, 10, 1) seq "WRONG") 
                              handle Subscript => "OK" | _ => "WRONG")
val test30f = tst0 "test30f" 
   ((case (Int.minInt, Int.maxInt) of
        (SOME min, SOME max) => 
            ((substring("", max, max); "WRONG")
             handle Subscript => 
                 ((substring("", min, min); "WRONG")
                  handle Subscript => "OK" | _ => "WRONG"))
       | _ => "OK")
            handle _ => "EXN")

(* val sa = "AAAAaAbAABBBB"; *)
val test31 = tst' "test31" 
    (fn _ => 
           isPrefix "" (substring(sa, 0, 0))
           andalso isPrefix "" (substring(sa, 13, 0))
           andalso isPrefix "" ssa1
           andalso isPrefix "aAbAA" ssa2
           andalso isPrefix "aAbA" ssa2
           andalso not (isPrefix "aAbAAB" ssa2)
           andalso not (isPrefix "aAbAAB" ssa1))

fun eqspan(sus1, sus2, res) = base(span(sus1, sus2)) = base res

val test32a = tst' "test32a" (fn _ =>
   eqspan(substring(sa, 0, 0), substring(sa, 0, 13), all sa)
   andalso eqspan(substring(sa, 0, 13), substring(sa, 13, 0), all sa)
   andalso eqspan(substring(sa, 5, 0), substring(sa, 5, 0), substring(sa, 5,0))
   andalso eqspan(substring(sa, 0, 5), substring(sa, 5, 8), all sa)
   andalso eqspan(substring(sa, 0, 13), substring(sa, 0, 13), all sa)
   andalso eqspan(substring(sa, 5, 4), substring(sa, 2, 4), substring(sa,5,1))
   andalso eqspan(substring(sa, 2, 5), substring(sa, 6, 3), substring(sa, 2,7))
   andalso eqspan(substring("abcd", 1, 0), substring("abcd", 1, 2), 
                  substring("abcd", 1, 2))
   andalso eqspan(substring("", 0, 0), substring("", 0, 0), all ""))

val test32b = tst0 "test32b" ((span(substring("a", 0, 0), substring("b", 0, 0)) seq "WRONG") 
                              handle Span => "OK" | _ => "WRONG")

val test32c = tst0 "test32c" ((span(substring(sa, 1, 0), substring(sa, 0, 0)) seq "WRONG") 
                              handle Span => "OK" | _ => "WRONG")

val test32d = tst0 "test32d" ((span(substring(sa, 3, 2), substring("abcd", 2, 1)) seq "WRONG") 
                              handle Span => "OK" | _ => "WRONG")

val test32e = tst0 "test32e" ((span(substring("a", 0, 0), substring("b", 0, 0)) seq "WRONG") 
                              handle Span => "OK" | _ => "WRONG")

end; 
