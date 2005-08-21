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

(* test/unixpath.sml 6 -- for Unix, 1995-05-23 *)

val _ = print "\nFile unixpath.sml: Testing structure Path...\n"

local 
    open OS.Path
    
val test1a = 
    tst' "test1a" (fn _ => fromString "" = {isAbs=false, vol = "", arcs = []});
val test1b = 
    tst' "test1b" (fn _ => fromString "/" = {isAbs=true, vol="", arcs=[""]});
val test1c = 
    tst' "test1c" (fn _ => fromString "//" = {isAbs=true, vol="", arcs=["", ""]});
val test1d = 
    tst' "test1d" (fn _ => fromString "a" = {isAbs=false, vol = "", arcs = ["a"]});
val test1e = 
    tst' "test1e" (fn _ => fromString "/a" = {isAbs=true, vol="", arcs=["a"]});
val test1f = 
    tst' "test1f" (fn _ => fromString "//a" = {isAbs=true, vol="", arcs=["","a"]});
val test1g = 
    tst' "test1g" (fn _ => fromString "a/" = {isAbs=false, vol = "", arcs = ["a", ""]});
val test1h = 
    tst' "test1h" (fn _ => fromString "a//" = {isAbs=false, vol = "", arcs = ["a", "", ""]});
val test1i = 
    tst' "test1i" (fn _ => fromString "a/b" = {isAbs=false, vol = "", arcs = ["a", "b"]});
val test1j = 
    tst' "test1j" (fn _ => fromString "a.b/c" = {isAbs=false, vol = "", arcs = ["a.b", "c"]});
val test1k = 
    tst' "test1k" (fn _ => fromString "a.b/c/" = {isAbs=false, vol = "", arcs = ["a.b", "c", ""]});
val test1l = 
    tst' "test1l" (fn _ => fromString "a/./c" = {isAbs=false, vol = "", arcs = ["a", ".", "c"]});
val test1m = 
    tst' "test1m" (fn _ => fromString "a/../c" = {isAbs=false, vol = "", arcs = ["a", "..", "c"]});
val test1n = 
    tst' "test1n" (fn _ => fromString "." = {isAbs=false, vol = "", arcs = ["."]});

val test2a =
    tst' "test2a" (fn _ => toString {isAbs=false, vol = "", arcs = []} = "");
val test2b = 
    tst' "test2b" (fn _ => toString {isAbs=true, vol="", arcs=[]} = "/");
val test2c = 
    tst' "test2c" (fn _ => toString {isAbs=true, vol="", arcs=["", ""]} = "//");
val test2d = 
    tst' "test2d" (fn _ => toString {isAbs=false, vol = "", arcs = ["a"]} = "a");
val test2e = 
    tst' "test2e" (fn _ => toString {isAbs=true, vol="", arcs=["a"]} = "/a");
val test2f = 
    tst' "test2f" (fn _ => toString {isAbs=true, vol="", arcs=["","a"]} = "//a");
val test2g = 
    tst' "test2g" (fn _ => toString {isAbs=false, vol = "", arcs = ["a", ""]} = "a/");
val test2h = 
    tst' "test2h" (fn _ => toString {isAbs=false, vol = "", arcs = ["a", "", ""]} = "a//");
val test2i = 
    tst' "test2i" (fn _ => toString {isAbs=false, vol = "", arcs = ["a", "b"]} = "a/b");
val test2j = 
    tst' "test2j" (fn _ => toString {isAbs=false, vol = "", arcs = ["a.b", "c"]} = "a.b/c");
val test2k = 
    tst' "test2k" (fn _ => toString {isAbs=false, vol = "", arcs = ["a.b", "c", ""]} = "a.b/c/");
val test2l = 
    tst' "test2l" (fn _ => toString {isAbs=false, vol = "", arcs = ["a", ".", "c"]} = "a/./c");
val test2m = 
    tst' "test2m" (fn _ => toString {isAbs=false, vol = "", arcs = ["a", "..", "c"]} = "a/../c");
val test2n = 
    tst' "test2n" (fn _ => toString {isAbs=true, vol="", arcs=["a", "..", "c"]} = "/a/../c");
val test2o = tst0 "test2o" ((toString {isAbs=false, vol = "", arcs =  ["", "a"]} seq "WRONG")
                            handle Path => "OK" | _ => "WRONG")
val test2p = tst0 "test2p" ((toString {isAbs=true, vol = "C:", arcs =  ["windows"]} seq "WRONG")
                            handle Path => "OK" | _ => "WRONG")

val test3b = 
    tst' "test3b" (fn _ => getVolume "/" = "");
val test3c = 
    tst' "test3c" (fn _ => getVolume "//" = "");
val test3d = 
    tst' "test3d" (fn _ => getVolume "a//b/c/" = "");
val test3e = 
    tst' "test3e" (fn _ => getVolume "./" = "");
val test3f = 
    tst' "test3f" (fn _ => getVolume "../" = "");
val test3g = 
    tst' "test3g" (fn _ => getVolume "" = "");
val test3h = 
    tst' "test3h" (fn _ => getVolume "C:" = "");

val test4a = 
    tst' "test4a" (fn _ => 
           List.all isRelative ["", ".", "..", "a//"]
           andalso not (List.exists isRelative ["/", "/a", "//"]));
val test4b = 
    tst' "test4b" (fn _ => 
           List.all isAbsolute ["/", "/a", "//", "/.", "/.."]
           andalso not (List.exists isAbsolute ["", ".", "..", "a//"]));

val test5a = 
    tst' "test5a" (fn _ => 
           getParent "/" = "/"
           andalso getParent "a" = "."
           andalso getParent "a/" = "a/.."
           andalso getParent "a///" = "a///.."
           andalso getParent "a/b" = "a"
           andalso getParent "a/b/" = "a/b/.."
           andalso getParent "/a/b" = "/a"
           andalso getParent "/a/b/" = "/a/b/.."
           andalso getParent ".." = "../.."
           andalso getParent "." = ".."
           andalso getParent "../" = "../.."
           andalso getParent "./" = "./.."
           andalso getParent "" = "..");

val test6a = 
    tst' "test6a" (fn _ => 
           concat("a", "b") = "a/b"
           andalso concat("a", "b/c") = "a/b/c"
           andalso concat("/", "b/c") = "/b/c"
           andalso concat("", "b/c") = "b/c"
           andalso concat("/a", "b/c") = "/a/b/c"
           andalso concat("a/", "b/c") = "a/b/c"
           andalso concat("a//", "b/c") = "a//b/c"
           andalso concat(".", "b/c") = "./b/c"
           andalso concat("a/b", "..") = "a/b/.."
           andalso concat("a/b", "../c") = "a/b/../c");
val test6b = tst0 "test6b" ((concat ("a", "/b") seq "WRONG")
                            handle Path => "OK" | _ => "WRONG")

val mkAbsolute = fn (p, r) => mkAbsolute {path = p, relativeTo = r}
   
val test7a = 
    tst' "test7a" (fn _ => 
           mkAbsolute("/a/b", "/c/d") = "/a/b"
           andalso mkAbsolute("/", "/c/d") = "/"
           andalso mkAbsolute("a/b", "/c/d") = "/c/d/a/b");
val test7b = tst0 "test7b" ((mkAbsolute("a", "c/d") seq "WRONG")
                            handle Path => "OK" | _ => "WRONG")
val test7c = tst0 "test7c" ((mkAbsolute("/a", "c/d") seq "WRONG")
                            handle Path => "OK" | _ => "WRONG")

val mkRelative = fn (p, r) => mkRelative {path = p, relativeTo = r}

val test8a = 
    tst' "test8a" (fn _ => 
           mkRelative("a/b", "/c/d") = "a/b"
           andalso mkRelative("/", "/a/b/c")       = "../../.." 
           andalso mkRelative("/a/", "/a/b/c")     = "../../" 
           andalso mkRelative("/a/b/", "/a/c")     = "../b/"     
           andalso mkRelative("/a/b", "/a/c/")     = "../b"      
           andalso mkRelative("/a/b/", "/a/c/")    = "../b/"     
           andalso mkRelative("/", "/")            = "."              
           andalso mkRelative("/", "/.")           = "."              
           andalso mkRelative("/", "/..")          = "."              
           andalso mkRelative("/", "/a")           = ".."             
           andalso mkRelative("/a/b/../c", "/a/d") = "../b/../c" 
           andalso mkRelative("/a/b", "/c/d")      = "../../a/b"
           andalso mkRelative("/c/a/b", "/c/d")    = "../a/b"
           andalso mkRelative("/c/d/a/b", "/c/d")  = "a/b");
val test8b = tst0 "test8b" ((mkRelative("/a", "c/d") seq "WRONG")
                            handle Path => "OK" | _ => "WRONG")
val test8c = tst0 "test8c" ((mkRelative("a", "c/d") seq "WRONG")
                            handle Path => "OK" | _ => "WRONG")

val test9a = let
    fun chkCanon (a, b) =
          (mkCanonical a = b) 
          andalso (mkCanonical b = b)
          andalso (isCanonical b)
    in
      tst' "test9a" (fn _ => 
           chkCanon("", ".")
           andalso chkCanon(".", ".")
           andalso chkCanon("./.", ".")
           andalso chkCanon("/.", "/")
           andalso chkCanon("..", "..")
           andalso chkCanon("../..", "../..")
           andalso chkCanon("b", "b")
           andalso chkCanon("a/b", "a/b")
           andalso chkCanon("/a/b", "/a/b")
           andalso chkCanon("a/b/", "a/b")
           andalso chkCanon("a/b//", "a/b")
           andalso chkCanon("a/../b", "b")
           andalso chkCanon("a/..", ".")
           andalso chkCanon("a/.", "a")
           andalso chkCanon("a/", "a")
           andalso chkCanon("/a/../b/", "/b")
           andalso chkCanon("/..", "/")
           andalso chkCanon("/../../a/b", "/a/b")
           andalso chkCanon("/./../../a/b", "/a/b")
           andalso chkCanon("/./../..", "/")
           andalso chkCanon("a/../b", "b")
           andalso chkCanon("a/./b", "a/b")
           andalso chkCanon("a////b", "a/b")
           andalso chkCanon("a////b", "a/b"))
    end

val test10a = 
    tst' "test10a" (fn _ => 
           not (isCanonical "./."
                orelse isCanonical "/.."
                orelse isCanonical "/."
                orelse isCanonical "//"
                orelse isCanonical "a/.."
                orelse isCanonical "a//b"
                orelse isCanonical "a/."
                orelse isCanonical "a/b/"
                orelse isCanonical "a/.."))
                
val test11a = 
    tst' "test11a" (fn _ => 
           splitDirFile "" = {dir = "", file = ""}
           andalso splitDirFile "." = {dir = "", file = "."}
           andalso splitDirFile ".." = {dir = "", file = ".."}
           andalso splitDirFile "b" = {dir = "", file = "b"}
           andalso splitDirFile "b/" = {dir = "b", file = ""}
           andalso splitDirFile "a/b" = {dir = "a", file = "b"}
           andalso splitDirFile "/a" = {dir = "/", file = "a"}
           andalso splitDirFile "/a/b" = {dir = "/a", file = "b"}
           andalso splitDirFile "/c/a/b" = {dir = "/c/a", file = "b"}
           andalso splitDirFile "/c/a/b/" = {dir = "/c/a/b", file = ""}
           andalso splitDirFile "/c/a/b.foo.bar" = {dir = "/c/a", file="b.foo.bar"}
           andalso splitDirFile "/c/a/b.foo" = {dir = "/c/a", file = "b.foo"});

(*    
val test11b = (splitDirFile "" seq "WRONG") 
              handle Path => "OK" | _ => "WRONG";
*)

val test12 = 
    tst' "test12" (fn _ =>
                   List.all (fn (res, dir, file) =>
                             res = joinDirFile {dir = dir, file = file})
                   [("", "", ""),
                    ("b", "", "b"),
                    ("/", "/", ""),
                    ("/b", "/", "b"),
                    ("a/b", "a", "b"),
                    ("/a/b", "/a", "b"),
                    ("/c/a/b", "/c/a", "b"),
                    ("/c/a/b/", "/c/a/b", ""),
                    ("/c/a/b.foo.bar", "/c/a","b.foo.bar"),
                    ("/c/a/b.foo", "/c/a", "b.foo")])

val test13 = 
    tst' "test13" (fn _ => 
           dir "b" = ""
           andalso dir "a/b" = "a"
           andalso dir "/" = "/"
           andalso dir "/b" = "/"
           andalso dir "/a/b" = "/a"
           andalso dir "/c/a/b" = "/c/a"
           andalso dir "/c/a/b/" = "/c/a/b"
           andalso dir "/c/a/b.foo.bar" = "/c/a"
           andalso dir "/c/a/b.foo" = "/c/a");

val test14 = 
    tst' "test14" (fn _ => 
           file "b" = "b"
           andalso file "a/b" = "b"
           andalso file "/" = ""
           andalso file "/b" = "b"
           andalso file "/a/b" = "b"
           andalso file "/c/a/b" = "b"
           andalso file "/c/a/b/" = ""
           andalso file "/c/a/b.foo.bar" = "b.foo.bar"
           andalso file "/c/a/b.foo" = "b.foo");

val test15 = 
    tst' "test15" (fn _ => 
           splitBaseExt "" = {base = "", ext = NONE}
           andalso splitBaseExt ".login" = {base = ".login", ext = NONE}
           andalso splitBaseExt "/.login" = {base = "/.login", ext = NONE}
           andalso splitBaseExt "a" = {base = "a", ext = NONE}
           andalso splitBaseExt "a." = {base = "a.", ext = NONE}
           andalso splitBaseExt "a.b" = {base = "a", ext = SOME "b"}
           andalso splitBaseExt "a.b.c" = {base = "a.b", ext = SOME "c"}
           andalso splitBaseExt "/a.b" = {base = "/a", ext = SOME "b"}
           andalso splitBaseExt "/c/a.b" = {base = "/c/a", ext = SOME "b"}
           andalso splitBaseExt "/c/a/b/.d" = {base = "/c/a/b/.d", ext = NONE}
           andalso splitBaseExt "/c.a/b.d" = {base = "/c.a/b", ext = SOME "d"}
           andalso splitBaseExt "/c.a/bd" = {base = "/c.a/bd", ext = NONE}
           andalso splitBaseExt "/c/a/b.foo.bar" = {base="/c/a/b.foo",ext=SOME "bar"}
           andalso splitBaseExt "/c/a/b.foo" = {base = "/c/a/b", ext = SOME "foo"});

val test16 = 
    tst' "test16" (fn _ => 
           "" = joinBaseExt {base = "", ext = NONE}
           andalso ".login" = joinBaseExt {base = ".login", ext = NONE}
           andalso "a" = joinBaseExt {base = "a", ext = NONE}
           andalso "a" = joinBaseExt {base = "a", ext = SOME ""}
           andalso "a.b" = joinBaseExt {base = "a", ext = SOME "b"}
           andalso "a.b.c" = joinBaseExt {base = "a.b", ext = SOME "c"}
           andalso "a.b.c.d" = joinBaseExt {base = "a.b", ext = SOME "c.d"}
           andalso "/a.b" = joinBaseExt {base = "/a", ext = SOME "b"}
           andalso "/c/a.b" = joinBaseExt {base = "/c/a", ext = SOME "b"}
           andalso "/c/a/b/.d" = joinBaseExt {base = "/c/a/b/", ext = SOME "d"}
           andalso "/c/a/b.foo.bar" = joinBaseExt {base="/c/a/b",ext=SOME "foo.bar"}
           andalso "/c/a/b.foo" = joinBaseExt {base = "/c/a/b", ext = SOME "foo"});

val test17 = 
    tst' "test17" (fn _ => 
           ext "" = NONE
           andalso ext ".login" = NONE
           andalso ext "/.login" = NONE
           andalso ext "a" = NONE
           andalso ext "a." = NONE
           andalso ext "a.b" = SOME "b"
           andalso ext "a.b.c" = SOME "c"
           andalso ext "a.b.c.d" = SOME "d"
           andalso ext "/a.b" = SOME "b"
           andalso ext "/c/a.b" = SOME "b"
           andalso ext "/c/a/b/.d" = NONE
           andalso ext "/c.a/b.d" = SOME "d"
           andalso ext "/c.a/bd" = NONE
           andalso ext "/c/a/b.foo.bar" = SOME "bar"
           andalso ext "/c/a/b.foo" = SOME "foo");

val test18 = 
    tst' "test18" (fn _ => 
           base "" = ""
           andalso base ".d" = ".d"
           andalso base ".login" = ".login"
           andalso base "/.login" = "/.login"
           andalso base "a" = "a"
           andalso base "a." = "a."
           andalso base "a.b" = "a"
           andalso base "a.b.c" = "a.b" 
           andalso base "a.b.c.d" = "a.b.c"
           andalso base "/a.b" = "/a"
           andalso base "/c/a.b" = "/c/a"
           andalso base "/c/a/b/.d" = "/c/a/b/.d"
           andalso base "/c.a/b.d" = "/c.a/b"
           andalso base "/c.a/bd" = "/c.a/bd"
           andalso base "/c/a/b.foo.bar" = "/c/a/b.foo"
           andalso base "/c/a/b.foo" = "/c/a/b");

val test19 = 
    tst' "test19" (fn () => validVolume{isAbs=false, vol=""}
           andalso validVolume{isAbs=true, vol=""}
           andalso not (validVolume{isAbs=true, vol="/"}
                        orelse validVolume{isAbs=false, vol="/"} 
                        orelse validVolume{isAbs=true, vol="C:"}
                        orelse validVolume{isAbs=false, vol="C:"}
                        orelse validVolume{isAbs=true, vol=" "}
                        orelse validVolume{isAbs=false, vol=" "})); 
in
end
