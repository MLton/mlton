fun test (str : string) =
    let open MLton.Finalizable
        val x = new str
    in addFinalizer (x, fn s => print (s ^ ": finalizer\n"));
       withValue (x, fn s =>
                        (print "before GC 5\n";
                         MLton.GC.collect ();
                         print "after GC 5\n";
                         (fn () => (print "invoking touch\n"; touch x))))
    end

val _ = (print "before test 5\n";
         let val t = test "test 5"
         in print "before GC 5a\n";
            MLton.GC.collect ();
            print "after GC 5a\n";
            t ();
            print "before GC 5b\n";
            MLton.GC.collect ();
            print "after GC 5b\n"
         end;
         print "before GC 5c\n";
         MLton.GC.collect ();
         print "after GC 5c\n")
