fun test (str : string) =
    let open MLton.Finalizable
        val x = new str
        exception Exit
    in addFinalizer (x, fn s => print (s ^ ": finalizer\n"));
       withValue (x, fn s =>
                        (print "before GC 6\n";
                         MLton.GC.collect ();
                         print "after GC 6\n";
                         raise Exit))
       handle Exit => ()
    end

val _ = (print "before test 6\n";
         test "test 6";
         print "before GC 6a\n";
         MLton.GC.collect ();
         print "after GC 6a\n")
