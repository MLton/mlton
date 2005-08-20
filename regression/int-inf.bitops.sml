fun pr i = (print (IntInf.fmt StringCvt.HEX i);
            print "\n")

fun prBin oper a b c =
  (print "    A: ";
   pr a;
   print "    B: ";
   pr b;
   print ("A " ^ oper ^ " B: ");
   pr c)

fun prUn oper a c =
  (print "  A: ";
   pr a;
   print (oper ^ " A: ");
   pr c)

fun prSh oper w a c =
  let
    val s = Word.fmt StringCvt.HEX w
    val sp = CharVector.tabulate(String.size s, fn _ => #" ")
  in
    print ("     " ^ sp ^ "A: ");
    pr a;
    print ("A " ^ oper ^ " " ^ s ^ ": ");
    pr c
  end

fun mkInt i n = if n = 0
                  then i
                  else mkInt (IntInf.+ (IntInf.* (i, IntInf.fromInt 10),i)) 
                             (n - 1)

val mkInt = fn i => fn n => mkInt (IntInf.fromInt i) n

fun tryBin' (a, b) =
  let
    val _ = prBin "&" a b (IntInf.andb (a, b))
    val _ = prBin "|" a b (IntInf.orb (a, b))
    val _ = prBin "^" a b (IntInf.xorb (a, b))
  in
    ()
  end
fun tryBin (a, b) =
  let
    val _ = tryBin' (a, b)
    val _ = tryBin' (IntInf.~ a, b)
    val _ = tryBin' (a, IntInf.~ b)
    val _ = tryBin' (IntInf.~ a, IntInf.~ b)
  in
    ()
  end
fun tryUn' a =
  let
    val _ = prUn "!" a (IntInf.notb a)
  in
    ()
  end
fun tryUn a =
  let
    val _ = tryUn' a
    val _ = tryUn' (IntInf.~ a)
  in
    ()
  end
fun trySh' a =
  let
    val _ = List.app (fn w => prSh "~>>" w a (IntInf.~>> (a, w)))
                     [0wx0, 0wx1, 0wx2, 0wxF, 0wx10, 0wx11, 0wx12]
    val _ = List.app (fn w => prSh " <<" w a (IntInf.<< (a, w)))
                     [0wx0, 0wx1, 0wx2, 0wxF, 0wx10, 0wx11, 0wx12]
  in
    ()
  end
fun trySh a =
  let
(*
    val _ = trySh' a
*)
    val _ = trySh' (IntInf.~ a)
  in
    ()
  end

fun loop (n', m') (n, m) =
  let
    fun loop' i =
      let
        fun loop'' j = 
          if j > m
            then loop' (i + 1)
            else (tryBin (mkInt i j, mkInt i j);
                  tryBin (mkInt i j, mkInt (i + 1) j);
                  tryBin (mkInt i j, mkInt i (j + 1));
                  tryBin (mkInt i j, mkInt (i + 1) (j + 1));
                  tryUn (mkInt i j);
                  trySh (mkInt i j);
                  loop'' (j + 1))
      in
        if i > n
          then ()
          else loop'' m'
      end
  in
    loop' n'
  end
val _ = loop (0, 0) (3, 3)
val _ = loop (0, 8) (3, 10)
val _ = loop (0, 20) (3, 22)
val _ = loop (0, 30) (3, 31)