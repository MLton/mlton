program this (output)
   procedure addcor;
      var bins,start,i,last : integer;   level : real;
     begin  bins := trunc((r1+r2)*maxcor);
             if bins < 1 then bins := 1;
         start := round(d*maxcor) - bins div 2;
         level := mm/bins;
         last := start+bins; if last>maxcor then last := maxcor;
         corfarray[start] := corfarray[start]-level;
         corfarray[last] := corfarray[last]+level;
     end;
