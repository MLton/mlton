fun pr x = print (concat [Real.toString x, "\n"])
   
val {whole, frac} = Real.split 1234.5678
val _ = pr whole
val _ = pr frac
