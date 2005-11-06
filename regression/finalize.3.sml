fun test2 (str : string) =
    let open MLton.Finalizable
        val x = new str
    in addFinalizer (x, fn s => print (s ^ ": finalizer\n"));
       (fn () => (print "invoking touch\n"; touch x))
    end

val _ = (print "before test 4\n";
         let val t = test2 "test 4"
         in print "before GC 4a\n";
            MLton.GC.collect ();
            print "after GC 4a\n";
            t ();
            print "before GC 4b\n";
            MLton.GC.collect ();
            print "after GC 4b\n"
         end)
