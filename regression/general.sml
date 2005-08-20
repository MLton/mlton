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


(* General -- incomplete 1996-04-19, 1996-09-30, 1997-03-12 *)


val _ = print "\nFile general.sml: Testing structure General...\n"

exception NoExceptionRaised

fun getExn (f : unit -> 'a) = 
    (f (); NoExceptionRaised) handle e => e

fun prExn(exnStr, exn) =
    (print "Should be `"; print exnStr; print "':\n  ";
     print (exnName exn); print "\n  ";
     print (exnMessage exn); print "\n");

exception E1;
exception E2 = E1;

val _ = List.map prExn
    [("E1 or E2",  E2),
     ("Bind",      getExn(fn _ => let val true = false in () end)),
     ("Match",     getExn(fn _ => (fn true => ()) false)),
     ("Subscript", getExn(fn _ => Vector.sub(vector [], ~1))),
     ("Overflow",      getExn(fn _ => Array.array(Array.maxLen+1, ()))),
(*   ("Overflow",  getExn(fn _ => Math.exp 1E99)),
     ("Domain",    getExn(fn _ => Math.ln ~1.0)),
*)   ("Div",       getExn(fn _ => 1 div 0)),
     ("Chr",       getExn(fn _ => Char.chr 9999999)),
     ("Fail",      Fail "demo"),
     ("Option",    getExn(fn _ => valOf NONE)),
     ("Empty",     getExn(fn _ => List.hd []))
(*     ("SysErr",    getExn (fn _ => FileSys.modTime "exists.not")), *)
(*     ("Io",        getExn (fn _ => TextIO.openOut "."))*)
     ];
