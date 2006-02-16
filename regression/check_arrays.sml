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

(*check_arrays.sml  13/10/1997 22:13. tho.*)

fun impossible s = (print "ERROR : "; print s; print "\n")

val Char_prim_array_maxLen = 200*4-42;
val Poly_prim_array_maxLen = 200-42;

(*test Word8Array structure*)

val _ =
   (print "\nTesting structure Word8Array\n";
let
fun dot () = {}(*pr "."*)
fun phase s = {}(*pr s*)
fun try_with n =
(print ("\nNow I will try with a " ^ Int.toString n ^ "-array.");
 let val a = Word8Array.array (n, 0w42);
     val i = ref 0
 in
   phase "\ncheck 1:";
   i := 0;
   while (!i < n) do
     (dot ();
      if Word8Array.sub (a, !i) <> 0w42
      then impossible ("check 1 failed: it is "
                       ^ Int.toString (Word8.toInt (Word8Array.sub (a, !i))))
      else ();
      i := !i + 1);
   phase "\ncheck length:";
   if Word8Array.length a <> n then
     impossible ("length was "
                 ^ Int.toString (Word8Array.length a)
                 ^ " and not "
                 ^ Int.toString n)
   else ();
   phase "\ncheck foldr:";
   if (Word8Array.foldr (fn (e,a) => Word8.toInt e + a) 0 a) <> Word8Array.length a * 42 then
     impossible ("foldr check failed: it was "
                 ^ Int.toString (Word8Array.foldr (fn (e,a) => Word8.toInt e + a) 0 a)
                 ^ " and not "
                 ^ Int.toString (Word8Array.length a * 42))
   else ();
   phase "\ninit:";
   i := 0;
   while (!i < n) do
     (dot ();
      Word8Array.update (a, !i, 0w2 * (Word8.fromInt (!i) mod 0w20));
      i := !i + 1);
   phase "\ncheck 2:";
   i := n-1;
   while (!i >= 0) do
     (dot ();
      if Word8Array.sub (a, !i) <> (0w2 * (Word8.fromInt (!i) mod 0w20))
      then impossible (concat["check 2 failed: found ",
                              (Int.toString o Word8.toInt)(Word8Array.sub (a, !i)),
                              " and not ",
                              (Int.toString o Word8.toInt)(0w2 * (Word8.fromInt (!i) mod 0w20))])
      else ();
      i := !i - 1);
   print "    \tok"
 end);
in
(try_with 119;
 try_with 13;
 try_with 130;
 try_with 10000;
 try_with 0;
 try_with 1;
 try_with Poly_prim_array_maxLen;
 try_with (2 * Poly_prim_array_maxLen);
 try_with (Poly_prim_array_maxLen + 1);
 try_with (20 * Poly_prim_array_maxLen + 1);
 try_with (20 * Char_prim_array_maxLen + 1);
 print "\n")
end

)

val _ =
   (
(*test Array structure*)
print "\nTesting structure Array\n";
let
fun dot () = () (*pr ".";*)
fun phase s = () (*pr s;*)
fun try_with n =
(print ("\nNow I will try with a " ^ Int.toString n ^ "-array.");
 let val a = Array.array (n, 42)
     val i = ref 0
 in
   phase "\ncheck 1:";
   i := 0;
   while (!i < n) do
     (dot ();
      if Array.sub (a, !i) <> 42 then impossible "check 1 failed"
      else ();
      i := !i + 1);
   phase "\ncheck length:";
   if Array.length a <> n then
     impossible ("length was "
                 ^ Int.toString (Array.length a)
                 ^ " and not "
                 ^ Int.toString n)
   else ();
   phase "\ncheck foldr:";
   if Array.foldr (op +) 0 a <> Array.length a * 42 then
     impossible ("foldr check failed: it was "
                 ^ Int.toString (Array.foldr (op +) 0 a)
                 ^ " and not "
                 ^ Int.toString (Array.length a * 42))
   else ();
   phase "\ninit:";
   i := 0;
   while (!i < n) do
     (dot ();
      Array.update (a, !i, !i * !i);
      i := !i + 1);
   phase "\ncheck 2:";
   i := n-1;
   while (!i >= 0) do
     (dot ();
      if Array.sub (a, !i) <> !i * !i then impossible "check 2 failed"
      else ();
      i := !i - 1);
   print "    \tok"
 end);
in
(try_with 119;
 try_with 13;
 try_with 130;
 try_with 10000;
 try_with 0;
 try_with 1;
 try_with Poly_prim_array_maxLen;
 try_with (2 * Poly_prim_array_maxLen);
 try_with (Poly_prim_array_maxLen + 1);
 try_with (20 * Poly_prim_array_maxLen + 1);
 try_with (20 * Char_prim_array_maxLen + 1);
 print "\n")
end
)



   val _ = (
(*test CharArray structure*)

print "\nTesting structure CharArray\n";
let
fun dot () = () (*pr ".";*)
fun phase s = () (*pr s*)
val x = #"*"
fun f (* : (elem * 'b) -> 'b *)  (x', b) = x = x' andalso b
val b_init = true
fun repeat x 0 = []
  | repeat x n = x :: repeat x (n-1)
fun try_with n =
(print ("\nNow I will try with a " ^ Int.toString n ^ "-array.");
 let val a = CharArray.array (n, x)
     val x_summasumarum = true
     val i = ref 0
 in
   phase "\ncheck 1:";
   i := 0;
   while (!i < n) do
     (dot ();
      if CharArray.sub (a, !i) <> x then impossible "check 1 failed"
      else ();
      i := !i + 1);
   phase "\ncheck length:";
   if CharArray.length a <> n then
     impossible ("length was "
                 ^ Int.toString (CharArray.length a)
                 ^ " and not "
                 ^ Int.toString n)
   else ();
   phase "\ncheck foldr:";
   if CharArray.foldr f b_init a <> x_summasumarum then
     impossible "foldr check failed"
   else ();
   phase "\ninit:";
   i := 0;
   while (!i < n) do
     (dot ();
      CharArray.update (a, !i, chr ((!i mod (127-34)) + 34) );
      i := !i + 1);
   phase "\ncheck 2:";
   i := n-1;
   while (!i >= 0) do
     (dot ();
      if CharArray.sub (a, !i) <> chr ((!i mod (127-34)) + 34)
      then impossible "check 2 failed"
      else ();
      i := !i - 1);
   print "    \tok"
 end);
in
(try_with 119;
 try_with 13;
 try_with 130;
 try_with 10000;
 try_with 0;
 try_with 1;
 try_with Char_prim_array_maxLen;
 try_with (2 * Char_prim_array_maxLen);
 try_with (Char_prim_array_maxLen + 1);
 try_with (20 * Char_prim_array_maxLen + 1);
 print "\n")
end

;
print "\ncheck done\n"
)


