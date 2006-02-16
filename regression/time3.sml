open Time

fun pt t = (print(toString t) ; print "\n")

val messy = fromReal 123.456789
   
val _ = (pt zeroTime;
         pt messy;
         app (fn d => (print(fmt d messy) ; print "\n")) [0,1,2,3,4,5,6,7];
         pt(fromReal 123.456789);
         pt(fromSeconds 123);
         pt(fromMilliseconds 123456);
         pt(fromMicroseconds 123456789)
         )
