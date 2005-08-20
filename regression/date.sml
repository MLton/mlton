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

(* test/date.sml
   PS 1995-03-20, 1995-05-12, 1996-07-05, 1998-04-07
*)

(* MosML test file ported to the ML Kit; ME 1998-07-17 *)

val _ = print "\nFile date.sml: Testing structure Date...\n"

local
    open Time Date 
    fun later h = 
        toString(fromTimeLocal(now() + fromReal (3600.0 * real h))) ^ "\n";
    fun nowdate () = Date.fromTimeLocal(now());
    fun mkdate(y,mo,d,h,mi,s) =
        date {year=y, month=mo, day=d, hour=h, minute=mi, second=s,
              offset = NONE}
    fun cmp(dt1, dt2) = compare(mkdate dt1, mkdate dt2)

    fun fromto dt = 
        toString (valOf (fromString (toString dt))) = toString dt

    fun tofrom s =
        toString (valOf (fromString s)) = s

    val y2k = 
        date {year=2000, month=Jan, day=1, hour=0, minute=0, second=0,
              offset = SOME Time.zeroTime}

    val y2kE1 = 
        date {year=2000, month=Jan, day=1, hour=0, minute=0, second=0,
              offset = SOME (Time.fromSeconds 82800) }

    val y2kW1 = 
        date {year=2000, month=Jan, day=1, hour=0, minute=0, second=0,
              offset = SOME (Time.fromSeconds 3600) }
    
val _ = 
    ((*print "This is (local time) now:        "; print (later 0);
     print "This is UTC now:                 "; 
     print (toString (fromTimeUniv(now()))); print "\n";
     print "This is an hour from now:        "; print (later 1);
     print "This is a day from now:          "; print (later 24);
     print "This is a week from now:         "; print (later 168);
     print "This is 120 days from now:       "; print (later (24 * 120));
     print "This is 160 days from now:       "; print (later (24 * 160));
     print "This is 200 days from now:       "; print (later (24 * 200));
     print "This is 240 days from now:       "; print (later (24 * 240));
*)
     print "This is the epoch (UTC):         "; 
     print (toString(fromTimeUniv zeroTime) ^ "\n");   
     print "The UTC millenium (UTC time):    "; 
     print (toString y2k ^ "\n");   
     print "The UTC millenium minus 5 sec:   "; 
     print (toString (date {year=2000, month=Jan, day=1, hour=0, 
                            minute=0, second= ~5, offset = SOME Time.zeroTime})
            ^ "\n")
(*     print "The UTC millenium (local time):  "; 
     print (toString (fromTimeLocal (toTime y2k)) ^ "\n");   
     print "The local millenium (UTC time):  "; 
     print (toString (fromTimeUniv (toTime (mkdate(2000, Jan, 1, 0, 0, 0))))
            ^ "\n");   
     print "The UTC+01 millenium (UTC):      "; 
     print (toString (fromTimeUniv (toTime y2kE1)) ^ "\n");
     print "The UTC-01 millenium (UTC):      "; 
     print (toString (fromTimeUniv (toTime y2kW1)) ^ "\n");
     print "This is today's number:          "; 
     print (fmt "%j" (nowdate()) ^ " (internally: "); 
     print (Int.toString (yearDay (nowdate())) ^ ")\n");
     print "This is today's weekday:         ";
     print (fmt "%A" (nowdate()) ^ "\n");
     print "This is the name of this month:  ";
     print (fmt "%B" (nowdate()) ^ "\n");
     print "Today's ISO date:                ";
     print (fmt "%Y-%m-%d" (nowdate ()) ^ "\n")*))
    

val test1 = 
tst' "test1" (fn _ => 
               cmp((1993,Jul,25,16,12,18), (1994,Jun,25,16,12,18)) = LESS
       andalso cmp((1995,May,25,16,12,18), (1994,Jun,25,16,12,18)) = GREATER
       andalso cmp((1994,May,26,16,12,18), (1994,Jun,25,16,12,18)) = LESS
       andalso cmp((1994,Jul,24,16,12,18), (1994,Jun,25,16,12,18)) = GREATER
       andalso cmp((1994,Jun,24,17,12,18), (1994,Jun,25,16,12,18)) = LESS
       andalso cmp((1994,Jun,26,15,12,18), (1994,Jun,25,16,12,18)) = GREATER
       andalso cmp((1994,Jun,25,15,13,18), (1994,Jun,25,16,12,18)) = LESS
       andalso cmp((1994,Jun,25,17,11,18), (1994,Jun,25,16,12,18)) = GREATER
       andalso cmp((1994,Jun,25,16,11,19), (1994,Jun,25,16,12,18)) = LESS
       andalso cmp((1994,Jun,25,16,13,17), (1994,Jun,25,16,12,18)) = GREATER
       andalso cmp((1994,Jun,25,16,12,17), (1994,Jun,25,16,12,18)) = LESS
       andalso cmp((1994,Jun,25,16,12,19), (1994,Jun,25,16,12,18)) = GREATER
       andalso cmp((1994,Jun,25,16,12,18), (1994,Jun,25,16,12,18)) = EQUAL);

val test2 = 
    tst' "test2" (fn _ => 
           fmt "%A" (mkdate(1995,May,22,4,0,1)) = "Monday");

val test3 = 
    tst' "test3" (fn _ => 
           List.all fromto 
           [mkdate(1995,Aug,22,4,0,1),
            mkdate(1996,Apr, 5, 0, 7, 21),
            mkdate(1996,Mar, 5, 6, 13, 58)]);

val test4 = 
    tst' "test4" (fn _ => 
           List.all tofrom 
           ["Fri Jul 05 14:25:16 1996",
           "Mon Feb 05 04:25:16 1996",
           "Sat Jan 06 04:25:16 1996"])

val test5 = 
    tst' "test5" (fn _ => 
           weekDay(mkdate(1962, Jun, 25, 1, 2, 3)) = Mon
           andalso weekDay(mkdate(1998, Mar, 6, 1, 2, 3)) = Fri
           andalso weekDay(mkdate(1998, Apr, 6, 1, 2, 3)) = Mon
           andalso weekDay(mkdate(1900, Feb, 28, 1, 2, 3)) = Wed
           andalso weekDay(mkdate(1900, Mar, 1, 1, 2, 3)) = Thu
           andalso weekDay(mkdate(1850, Feb, 28, 1, 2, 3)) = Thu
           andalso weekDay(mkdate(1850, Mar, 1, 1, 2, 3)) = Fri
           andalso weekDay(mkdate(1860, Feb, 28, 1, 2, 3)) = Tue
           andalso weekDay(mkdate(1860, Feb, 29, 1, 2, 3)) = Wed
           andalso weekDay(mkdate(1860, Mar, 1, 1, 2, 3)) = Thu
           andalso weekDay(mkdate(2000, Feb, 28, 1, 2, 3)) = Mon
           andalso weekDay(mkdate(2000, Feb, 29, 1, 2, 3)) = Tue
           andalso weekDay(mkdate(2000, Mar, 1, 1, 2, 3)) = Wed)

val test6 = 
    tst' "test6" (fn _ => 
           yearDay(mkdate(1962, Jan, 1, 1, 2, 3)) = 0
           andalso yearDay(mkdate(1998, Mar, 6, 1, 2, 3)) = 64
           andalso yearDay(mkdate(1900, Feb, 28, 1, 2, 3)) = 58
           andalso yearDay(mkdate(1900, Mar, 1, 1, 2, 3)) = 59
           andalso yearDay(mkdate(1900, Dec, 31, 1, 2, 3)) = 364
           andalso yearDay(mkdate(1850, Feb, 28, 1, 2, 3)) = 58
           andalso yearDay(mkdate(1850, Mar, 1, 1, 2, 3)) = 59
           andalso yearDay(mkdate(1850, Dec, 31, 1, 2, 3)) = 364
           andalso yearDay(mkdate(1860, Feb, 28, 1, 2, 3)) = 58
           andalso yearDay(mkdate(1860, Feb, 29, 1, 2, 3)) = 59
           andalso yearDay(mkdate(1860, Mar, 1, 1, 2, 3)) = 60
           andalso yearDay(mkdate(1860, Dec, 31, 1, 2, 3)) = 365
           andalso yearDay(mkdate(2000, Feb, 28, 1, 2, 3)) = 58
           andalso yearDay(mkdate(2000, Feb, 29, 1, 2, 3)) = 59
           andalso yearDay(mkdate(2000, Mar, 1, 1, 2, 3)) = 60
           andalso yearDay(mkdate(2000, Dec, 31, 1, 2, 3)) = 365
           andalso yearDay(mkdate(1959, Feb, 28, 1, 2, 3)) = 58
           andalso yearDay(mkdate(1959, Mar, 1, 1, 2, 3)) = 59
           andalso yearDay(mkdate(1959, Dec, 31, 1, 2, 3)) = 364
           andalso yearDay(mkdate(1960, Feb, 28, 1, 2, 3)) = 58
           andalso yearDay(mkdate(1960, Feb, 29, 1, 2, 3)) = 59
           andalso yearDay(mkdate(1960, Mar, 1, 1, 2, 3)) = 60
           andalso yearDay(mkdate(1960, Dec, 31, 1, 2, 3)) = 365)

fun addh h = 
    let val dt = mkdate(1998, Apr, 6, h, 0, 0)
    in (month dt, day dt, hour dt) end

val test7 = 
    tst' "test7" (fn _ => 
           addh 0 = (Apr, 6, 0)
           andalso addh 23 = (Apr, 6, 23)
           andalso addh 24 = (Apr, 7, 0)
           andalso addh 36 = (Apr, 7, 12)
           andalso addh 600 = (May, 1, 0)
           andalso addh 610 = (May, 1, 10)
           andalso addh 625 = (May, 2, 1))

val test8 = 
    tst' "test8" (fn _ => 
           hour (mkdate(1998, Mar, 28, 12, 0, 0)) = 12
           andalso hour (mkdate(1998, Mar, 28, 36, 0, 0)) = 12)

in
end
