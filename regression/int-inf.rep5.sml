structure II = IntInf
structure MII = MLton.IntInf

structure BigWord = MLton.IntInf.BigWord
structure SmallInt = MLton.IntInf.SmallInt
datatype rep = datatype MLton.IntInf.rep
val toRep = MLton.IntInf.rep
val fromRep = MLton.IntInf.fromRep

fun checkToFrom ii =
  let
     fun bug str =
        print (concat ["checkToFrom ",
                       IntInf.toString ii,
                       " => ",
                       str,
                       "\n"])
     val r = toRep ii
     val () =
        if false
           then print (concat ["toRep ", IntInf.toString ii, " = ",
                               case r of
                                  Big _ => "Big\n"
                                | Small _ => "Small\n"])
        else ()
  in
     case fromRep r of
        NONE => bug "(isSome (fromRep r)) failed"
      | SOME ii' =>
           if ii = ii'
              then if r = toRep ii'
                      then ()
                   else bug "(r = (toRep ii')) failed"
           else bug "(ii = ii') failed"
  end

fun loop l =
   case l of
      nil => ()
    | (lo,hi)::l =>
         let
            fun iloop ii =
               if ii <= hi
                  then (checkToFrom ii; iloop (ii + 1))
               else loop l
         in
            iloop lo
         end

val tests =
   let
      val op + = SmallInt.+
      val op - = SmallInt.-
      val op div = SmallInt.div
      val two = SmallInt.fromInt 2
      val thirtytwo = SmallInt.fromInt 32
      val sixtyfour = thirtytwo + thirtytwo

      val min = valOf SmallInt.minInt
      val hmin = min div two
      val max = valOf SmallInt.maxInt
      val hmax = max div two
   in
      [(SmallInt.toLarge min, SmallInt.toLarge (min + sixtyfour)),
       (SmallInt.toLarge (hmin - thirtytwo), SmallInt.toLarge (hmin + thirtytwo)),
       (SmallInt.toLarge (hmax - thirtytwo), SmallInt.toLarge (hmax + thirtytwo)),
       (SmallInt.toLarge (max - sixtyfour), SmallInt.toLarge max)]
   end
   @
   let
      val prec = valOf SmallInt.precision
      val min = ~ (IntInf.pow (2, prec - 1))
      val hmin = min div 2
      val max = IntInf.pow (2, prec - 1) - 1
      val hmax = max div 2
   in
      [(min, (min + 64)),
       ((hmin - 32), (hmin + 32)),
       ((hmax - 32), (hmax + 32)),
       ((max - 64), max)]
   end

val () = loop tests
