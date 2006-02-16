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

(* test/filesys.sml
   PS 1995-03-23, 1996-05-01, 1998-04-06
*)

(* DOS: Plain WRONG: test6a, test9a (and test9b);
        Excusable:   test8b, test11b, test12a, test13a, test13b, test13c
 *)

(* The test requires three symbolic links to be present in the current directory:
        testlink -> README
        testcycl -> testcycl 
        testbadl -> exists.not
   Moreover, the file README must exist and the file exists.not not.
   Also, the test requires one hard link between file hardlinkA and file hardlinkB. 
*)

val _ = print "\nFile filesys.sml: Testing structure FileSys...\n"

local
    open OS.FileSys
    (* Clean up: *)
    val _ = (rmDir "testdir") handle OS.SysErr _ => (); 
    val _ = (rmDir "testdir2") handle OS.SysErr _ => (); 

val test1a = tst0 "test1a" ((mkDir "testdir" seq "OK") handle _ => "WRONG")
val test1b = tst0 "test1b" ((mkDir "testdir" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")

val test2 = tst' "test2" (fn _ => isDir "testdir");
    
val test3a = tst' "test3a" (fn _ => access("testdir", [A_READ, A_EXEC, A_WRITE]));

local 
    val cdir = getDir();
in
    val test4a = tst0 "test4a" ((chDir cdir seq "OK") handle _ => "WRONG")
    val test4b = tst' "test4b" (fn _ => cdir = getDir());
    val _ = chDir "testdir";
    val test4c = tst' "test4c" (fn _ => cdir <> getDir());
    val _ = chDir "..";
    val test4d = tst' "test4d" (fn _ => cdir = getDir());
end;

val _ = rename{old = "testdir", new = "exists.not"};

val test5 = tst0 "test5" ((rmDir "exists.not" seq "OK") handle _ => "WRONG")

val test6a = tst0 "test6a" ((openDir "exists.not" seq "WRONG") 
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test6b = tst0 "test6b" ((isDir "exists.not" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test6c = tst0 "test6c" ((rmDir "exists.not" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test6d = tst0 "test6d" ((chDir "exists.not" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test6e = tst0 "test6e" ((fullPath "exists.not" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test6f = tst0 "test6f" ((realPath "exists.not" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test6g = tst0 "test6g" ((modTime "exists.not" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test6h = tst0 "test6h" ((setTime("exists.not", NONE) seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test6i = tst0 "test6i" ((remove "exists.not" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test6j = tst0 "test6j" ((rename{old="exists.not", new="testdir2"} seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test6k = tst0 "test6k" ((fileSize "exists.not" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test6l = tst' "test6l" (fn _ => not (access("exists.not", [])));

val _ = mkDir "testdir";

local 
    val dstr = openDir "testdir";
in
    val test7a = 
        tst' "test7a" (fn _ => NONE = readDir dstr);
    val _ = rewindDir dstr;
    val test7b = 
        tst' "test7b" (fn _ => NONE = readDir dstr);
    val _ = closeDir dstr;
    val test7c = tst0 "test7c" ((readDir dstr seq "WRONG")
                                handle OS.SysErr _ => "OK" | _ => "WRONG")
    val test7d = tst0 "test7d" ((rewindDir dstr seq "WRONG")
                                handle OS.SysErr _ => "OK" | _ => "WRONG")
    val test7e = tst0 "test7e" ((closeDir dstr seq "OK")
                                handle _ => "WRONG")
end

val _ =
   List.app
   (fn (new, old) =>
    if isLink new handle OS.SysErr _ => false
       then ()
    else Posix.FileSys.symlink {new = new, old = old})
   [("testlink", "README"),
    ("testcycl", "testcycl"),
    ("testbadl", "exists.not")]

val test8a = 
    tst' "test8a" (fn _ => fullPath "." = getDir ());
val test8b = 
    tst' "test8b" (fn _ => fullPath "testlink" = getDir() ^ "/README");
val test8c = tst0 "test8c" ((fullPath "testcycl" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test8d = tst0 "test8d" ((fullPath "testbadl" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test8e = tst' "test8e" (fn _ => realPath "." = ".");
val test8f = tst' "test8f" (fn _ => realPath "testlink" = "README");
val test8g = tst0 "test8g" ((realPath "testcycl" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")
val test8h = tst0 "test8h" ((realPath "testbadl" seq "WRONG")
                            handle OS.SysErr _ => "OK" | _ => "WRONG")

val test9a = 
    tst' "test9a" (fn _ => 
           setTime ("README", SOME (Time.fromReal 1E6)) = ());
val test9b = 
    tst' "test9b" (fn _ => modTime "README" = Time.fromReal 1E6);
    
val test10a = tst0 "test10a" ((remove "testdir" seq "WRONG")
                              handle OS.SysErr _ => "OK" | _ => "WRONG")
val test10b = 
    tst' "test10b" (fn _ => 
           rename{old = "testdir", new = "testdir2"} = ());
val test10c = 
    tst' "test10c" (fn _ => isDir "testdir2");

val test11a = 
    tst' "test11a" (fn _ => not (access ("testdir", [])));
val test11b = 
    tst' "test11b" (fn _ => access("testlink", []));
val test11c = 
    tst' "test11c" (fn _ => not (access("testbadl", [])));

val test12a = 
    tst' "test12a" (fn _ => isLink "testcycl" 
           andalso isLink "testlink"
           andalso isLink "testbadl");
val test12b = 
    tst' "test12b" (fn _ => not (isLink "testdir2"
                        orelse isLink "README"));
val test12c = tst0 "test12c" ((isLink "exists.not" seq "WRONG")
                              handle OS.SysErr _ => "OK" | _ => "WRONG")

val test13a = 
    tst' "test13a" (fn _ => readLink "testcycl" = "testcycl");
val test13b = 
    tst' "test13b" (fn _ => readLink "testlink" = "README");
val test13c = 
    tst' "test13c" (fn _ => readLink "testbadl" = "exists.not");
val test13d = tst0 "test13d" ((readLink "testdir2" seq "WRONG")
                              handle OS.SysErr _ => "OK" | _ => "WRONG")
val test13e = tst0 "test13e" ((readLink "exists.not" seq "WRONG")
                              handle OS.SysErr _ => "OK" | _ => "WRONG")

val test14 = tst0 "test14" ((tmpName () seq "OK"))

val test15a = 
    tst' "test15a" (fn _ => 
           fileId "." = fileId "."
           andalso fileId "testlink" = fileId "README"
           andalso fileId "." <> fileId "README");
val test15b = 
    tst' "test15b" (fn _ => compare(fileId ".", fileId ".") = EQUAL)
val test15b1 =
    tst' "test15b1" (fn _ => compare(fileId ".", fileId "README") <> EQUAL)
val test15b2 =
    tst' "test15b2" (fn _ => compare(fileId "testlink", fileId "README") = EQUAL)
val test15b3 =
    tst' "test15b3" (fn _ => 
                     (compare(fileId ".", fileId "README") = LESS 
                      andalso compare(fileId "README", fileId ".") = GREATER
                      orelse 
                      compare(fileId ".", fileId "README") = GREATER 
                      andalso compare(fileId "README", fileId ".") = LESS));
val test15c = tst0 "test15c" ((fileId "exists.not" seq "WRONG")
                              handle OS.SysErr _ => "OK" | _ => "WRONG")
val test15d = tst0 "test15d" ((fileId "testbadl" seq "WRONG")
                              handle OS.SysErr _ => "OK" | _ => "WRONG")
val test15e = tst0 "test15e" ((fileId "testcycl" seq "WRONG")
                              handle OS.SysErr _ => "OK" | _ => "WRONG")
(* Unix only: *)

val _ =
   (if access ("hardlinkA", [])
       then ()
    else TextIO.closeOut (TextIO.openOut "hardlinkA")
    ; if access ("hardlinkB", [])
         then ()
      else Posix.FileSys.link {old = "hardlinkA", new = "hardlinkB"})
   
val test15f = 
  tst' "test15f" (fn _ => fileId "hardlinkA" = fileId "hardlinkB")
val test15g =
  tst' "test15g" (fn _ => compare(fileId "hardlinkA", fileId "hardlinkB") = EQUAL)

val _ = rmDir "testdir2";
in
end
