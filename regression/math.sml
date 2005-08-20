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

(* test/math.sml 
   PS 1995-02-25, 1996-04-01, 1997-03-07
*)

val _ = print "\nFile math.sml: Testing structure Math...\n"

local
    open Math
    val MAXDOUBLE = 8.98846567431157E307;
    val MINDOUBLE = 4.94065645841246544E~324
    val PI = 3.14159265358979323846;
    val E = 2.7182818284590452354;

    val eps = 1E~8
    infix 4 ===
    fun x === y = 
        abs (x - y) <= eps orelse abs(x-y) <= eps * (abs x + abs y)

    fun check1 (opr, a, r) = if opr a === r then "OK" else "WRONG"
    fun check2 (opr, a1, a2, r) =
        if opr(a1, a2) === r then "OK" else "WRONG"
    fun tst1 s (opr, a, r) = tst0 s (check1 (opr, a, r))
    fun tst2 s (opr, a1, a2, r) = tst0 s (check2 (opr, a1, a2, r))

val test0a = tst "test0a" (PI === pi);
val test0b = tst "test0b" (E === e);

val test1a = tst1 "test1a" (sqrt, 64.0, 8.0);
val test1b = tst1 "test1b" (sqrt, 0.0, 0.0);
val test1c = tst0 "test1c" (if Real.isNan(sqrt ~1.0) then "OK" else "WRONG")

val test2a = tst1 "test2a" (sin, 0.0, 0.0);
val test2b = tst1 "test2b" (sin, pi/2.0, 1.0);
val test2c = tst1 "test2c" (sin, pi, 0.0);
val test2d = tst1 "test2d" (sin, 3.0*pi/2.0, ~1.0);

val test3a = tst1 "test3a" (cos, 0.0, 1.0);
val test3b = tst1 "test3b" (cos, pi/2.0, 0.0);
val test3c = tst1 "test3c" (cos, pi, ~1.0);
val test3d = tst1 "test3d" (cos, 3.0*pi/2.0, 0.0);

val test4a = tst1 "test4a" (tan, 0.0, 0.0);
val test4b = tst1 "test4b" (tan, pi/4.0, 1.0);
val test4c = tst1 "test4c" (tan, pi, 0.0);
val test4d = tst1 "test4d" (tan, 3.0*pi/4.0, ~1.0);
val test4e = tst1 "test4e" (tan, ~pi/4.0, ~1.0);
val test4f = tst "test4f" ((abs(tan (pi/2.0))  > 1E8) handle _ => true);
val test4g = tst "test4g" ((abs(tan (~pi/2.0)) > 1E8) handle _ => true);

val test5a = tst1 "test5a" (asin, 0.0, 0.0);
val test5b = tst1 "test5b" (asin, 1.0, pi/2.0);
val test5c = tst1 "test5c" (asin, ~1.0, ~pi/2.0);
val test5d = tst0 "test5d" (if Real.isNan(asin 1.1) then "OK" else "WRONG")
val test5e = tst0 "test5e" (if Real.isNan(asin ~1.1) then "OK" else "WRONG")

val test6a = tst1 "test6a" (acos, 1.0, 0.0);
val test6b = tst1 "test6b" (acos, 0.0, pi/2.0);
val test6c = tst1 "test6c" (acos, ~1.0, pi);
val test6d = tst0 "test6d" (if Real.isNan(acos 1.1) then "OK" else "WRONG")
val test6e = tst0 "test6e" (if Real.isNan(acos ~1.1) then "OK" else "WRONG")

val test7a = tst1 "test7a" (atan, 0.0, 0.0);
val test7b = tst1 "test7b" (atan, 1.0, pi/4.0);
val test7c = tst1 "test7c" (atan, ~1.0, ~pi/4.0);
val test7d = tst1 "test7d" (atan, 1E8, pi/2.0);
val test7e = tst1 "test7e" (atan, ~1E8, ~pi/2.0);

(* atan2 -- here I am in doubt over the argument order, since the New
Basis document is inconsistent with itself and with atan2 in the C
libraries. *)

val test8a = tst2 "test8a" (atan2, 0.0, 0.0, 0.0);
val test8b = tst2 "test8b" (atan2, 1.0, 0.0, pi/2.0);
val test8c = tst2 "test8c" (atan2, ~1.0, 0.0, ~pi/2.0);
val test8d = tst2 "test8d" (atan2, 1.0, 1.0, pi/4.0);
val test8e = tst2 "test8e" (atan2, ~1.0, 1.0, ~pi/4.0);
val test8f = tst2 "test8f" (atan2, ~1.0, ~1.0, ~3.0*pi/4.0);
val test8g = tst2 "test8g" (atan2, 1.0, ~1.0, 3.0*pi/4.0);
val test8h = tst2 "test8h" (atan2, 1E8, 1.0, pi/2.0);
val test8i = tst2 "test8i" (atan2, ~1E8, 1.0, ~pi/2.0);
val test8j = tst2 "test8j" (atan2, 1.0, 1E8, 0.0);
val test8k = tst2 "test8k" (atan2, 1.0, ~1E8, pi);
val test8l = tst2 "test8l" (atan2, ~1.0, ~1E8, ~pi);

val test9a = tst1 "test9a" (exp, 0.0, 1.0);
val test9b = tst1 "test9b" (exp, 1.0, e);
val test9c = tst1 "test9c" (exp, ~1.0, 1.0/e);

val test10a = tst1 "test10a" (ln, 1.0, 0.0);
val test10b = tst1 "test10b" (ln, e, 1.0);
val test10c = tst1 "test10c" (ln, 1.0/e, ~1.0);
val test10d = tst0 "test10d" (if Real.==(ln 0.0,Real.negInf) then "OK" else "WRONG")
val test10e = tst0 "test10e" (if Real.isNan(ln ~1.0) then "OK" else "WRONG")
val test10f = tst0 "test10f" (if Real.==(ln Real.posInf, Real.posInf) then "OK" else "WRONG")
val test10g = tst0 "test10g" (if Real.==(Real.posInf, Real.posInf) then "OK" else "WRONG")

val test12a = tst2 "test12a" (pow, 0.0, 0.0, 1.0); (* arbitrary, might be 0.0 *)
val test12b = tst2 "test12b" (pow, 7.0, 0.0, 1.0); 
val test12c = tst2 "test12c" (pow, 0.0, 7.0, 0.0); 
val test12d = tst2 "test12d" (pow, 64.0, 0.5, 8.0); 
val test12e = tst2 "test12e" (pow, ~9.0, 2.0, 81.0); 
val test12f = tst2 "test12f" (pow, 10.0, ~2.0, 0.01); 
val test12g = tst2 "test12g" (pow, ~10.0, ~2.0, 0.01); 
val test12h = tst2 "test12h" (pow, 0.0, 0.5, 0.0); 
val test12i = tst2 "test12i" (pow, 0.4, ~2.0, 6.25); 

(*we do not follow the Basis Library specification exactly here, but rather follow math.h
val test12j = tst0 "test12j" (if Real.==(pow(0.0, ~1.0),Real.posInf) then "OK" else "WRONG")
val test12k = tst0 "test12k" (if Real.==(pow(0.0, ~0.5),Real.posInf) then "OK" else "WRONG")
*)
val test12l = tst0 "test12l" (if Real.isNan(pow(~1.0, 1.1)) then "OK" else "WRONG")
val test12m = tst0 "test12m" (if Real.isNan(pow(~1.0, 0.5)) then "OK" else "WRONG")
(* sweeks removed 12n because it fails on FreeBSD on x86, apparently due to a
 * 64 bit vs 80 bit issue.
 *)
(* val test12n = tst0 "test12n" (if Real.==(pow(3.0, 1000000.0),Real.posInf) then "OK" else "WRONG") *)   (* not in basis lib spec.*)
val test13a = tst1 "test13a" (log10, 1.0, 0.0);
val test13b = tst1 "test13b" (log10, 10.0, 1.0);
val test13c = tst1 "test13c" (log10, 100.0, 2.0);
val test13d = tst1 "test13d" (log10, 0.1, ~1.0);
val test13e = tst1 "test13e" (log10, 0.01, ~2.0);

val check14a = tst1 "test14a" (sinh, 0.0, 0.0);
val check14b = tst1 "test14b" (sinh,  1.0, 1.17520119364);
val check14c = tst1 "test14c" (sinh, ~1.0, ~1.17520119364);
val check14d = tst1 "test14d" (sinh,  2.0,  3.62686040785);
val check14e = tst1 "test14e" (sinh, ~2.0, ~3.62686040785);

val check15a = tst1 "test15a" (cosh, 0.0, 1.0);
val check15b = tst1 "test15b" (cosh,  1.0, 1.54308063482);
val check15c = tst1 "test15c" (cosh, ~1.0, 1.54308063482);
val check15d = tst1 "test15d" (cosh,  2.0, 3.76219569108);
val check15e = tst1 "test15e" (cosh, ~2.0, 3.76219569108);

val check16a = tst1 "test16a" (tanh, 0.0, 0.0);
val check16b = tst1 "test16b" (tanh,  1.0,  0.761594155956);
val check16c = tst1 "test16c" (tanh, ~1.0, ~0.761594155956);
val check16d = tst1 "test16d" (tanh,  2.0,  0.964027580076);
val check16e = tst1 "test16e" (tanh, ~2.0, ~0.964027580076);
val check16f = tst1 "test16f" (tanh,  100.0,  1.0);
val check16g = tst1 "test16g" (tanh, ~100.0, ~1.0);
in
end
