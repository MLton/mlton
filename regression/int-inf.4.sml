fun dump (x: IntInf.int): unit =
       let val rest = IntInf.quot (x, 10)
       in (print o Int.toString o IntInf.toInt o IntInf.rem) (x, 10);
          if rest = 0
             then print "\n"
             else dump rest
       end
                  
val _ = dump 12345678901234567890
