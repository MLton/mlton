functor Test (W: WORD) =
struct

structure LW = LargeWord
   
val zero = W.fromInt 0
val one = W.fromInt 1
val two = W.fromInt 2
val max = W.~ one
   
val words =
   [max,
    W.- (max, one),
    W.div (max, two),
    W.fromInt 0xF,
    two,
    one,
    zero]

fun foreach (l, f) = List.app f l
   
fun for (f: W.word -> unit) = foreach (words, f)

structure Answer =
   struct
      datatype t =
         Div
       | Overflow
       | Word of W.word

      val toString =
         fn Div => "Div"
          | Overflow => "Overflow"
          | Word w => W.toString w

      fun run (f: unit -> W.word): t =
         Word (f ())
         handle General.Div => Div
              | General.Overflow => Overflow

      val equals: t * t -> bool = op =
   end

val m = concat ["Word", Int.toString W.wordSize]
   
val _ = print (concat ["Testing ", m, "\n"])

fun err msg = print (concat [m, ": ", concat msg, "\n"])

val _ = for (fn w =>
             print (concat [W.toString w, "\n",
                            "\t", W.fmt StringCvt.BIN w, "\n",
                            "\t", W.fmt StringCvt.OCT w, "\n",
                            "\t", W.fmt StringCvt.DEC w, "\n",
                            "\t", W.fmt StringCvt.HEX w, "\n"]))

val _ =
   foreach
   ([("+", W.+, LW.+),
     ("-", W.-, LW.-),
     ("*", W.*, LW.* ),
     ("andb", W.andb, LW.andb),
     ("div", W.div, LW.div),
     ("max", W.max, LW.max),
     ("min", W.min, LW.min),
     ("mod", W.mod, LW.mod),
     ("orb", W.orb, LW.orb),
     ("xorb", W.xorb, LW.xorb)],
    fn (name, f, f') =>
    for
    (fn w =>
     for
     (fn w' =>
      let
         val a = Answer.run (fn () => f (w, w'))
         val a' = Answer.run (fn () =>
                              W.fromLarge (f' (W.toLarge w, W.toLarge w')))

      in
         if Answer.equals (a, a')
            then ()
         else err [W.toString w, " ", name, " ", W.toString w',
                   " = ", Answer.toString a, " <> ", Answer.toString a']
      end)))

val _ =
   for (fn w =>
        if w = valOf (W.fromString (W.toString w))
           then ()
        else err ["{from,to}String"])
   
val _ =
   foreach
   ([("<<", W.<<, LW.<<),
     (">>", W.>>, LW.>>)],
    fn (name, f, f') =>
    for
    (fn w =>
     foreach
     ([0w0, 0w1, 0w2, 0w4, 0w8, 0w15, 0w30, 0wxFF],
      fn w' =>
      let
         val a = f (w, w')
         val a' = W.fromLarge (f' (W.toLarge w, w'))
      in
         if a = a'
            then ()
         else err [W.toString w, " ", name, " ", Word.toString w',
                   " = ", W.toString a, " <> ", W.toString a']
      end)))

val _ =
   foreach
   ([("~>>", W.~>>, LW.~>>)],
    fn (name, f, f') =>
    for
    (fn w =>
     foreach
     ([0w0, 0w1, 0w2, 0w4, 0w8, 0w15, 0w30, 0wxFF],
      fn w' =>
      let
         val a = f (w, w')
         val a' = W.fromLarge (f' (W.toLargeX w, w'))
      in
         if a = a'
            then ()
         else err [W.toString w, " ", name, " ", Word.toString w',
                   " = ", W.toString a, " <> ", W.toString a']
      end)))

val _ =
   foreach
   ([("<", W.<, LW.<),
     ("<=", W.<=, LW.<=),
     (">", W.>, LW.>),
     (">=", W.>=, LW.>=)],
    fn (name, f, f') =>
    for
    (fn w =>
     for
     (fn w' =>
      let
         val b = f (w, w')
         val b' = f' (W.toLarge w, W.toLarge w')
      in
         if b = b'
            then ()
         else err [W.toString w, " ", name, " ", W.toString w',
                   " = ", Bool.toString b, " <> ", Bool.toString b']
      end)))

val _ =
   foreach
   ([("compare", W.compare, LW.compare)],
    fn (name, f, f') =>
    for
    (fn w =>
     for
     (fn w' =>
      let
         val or = f (w, w')
         val or' = f' (W.toLarge w, W.toLarge w')
      in
         if or = or'
            then ()
         else err [W.toString w, " ", name, " ", W.toString w']
      end)))

val _ =
   for
   (fn w =>
    if w = W.fromLargeInt (W.toLargeInt w)
       andalso w = W.fromLargeInt (W.toLargeIntX w)
       andalso (case SOME (W.toInt w) handle Overflow => NONE of
                   NONE => true
                 | SOME i => w = W.fromInt i)
       andalso (case SOME (W.toIntX w) handle Overflow => NONE of
                   NONE => true
                 | SOME i => w = W.fromInt i)
       then ()
    else err ["{from,to}Large"])

val _ =
   for (fn w =>
        let
           val a = W.notb w
           val a' = W.fromLarge (LW.notb (W.toLarge w))
        in
           if a = a'
              then ()
           else err ["notb ", W.toString w, " = ", W.toString a, " <> ",
                     W.toString a']
        end)

val _ =
   for (fn w =>
        if W.~ w = W.- (zero, w)
           then ()
        else err ["~"])

end

structure Z = Test (Word2)
structure Z = Test (Word3)
structure Z = Test (Word4)
structure Z = Test (Word7)
structure Z = Test (Word8)
structure Z = Test (Word9)
structure Z = Test (Word13)
structure Z = Test (Word16)
structure Z = Test (Word17)
structure Z = Test (Word20)
structure Z = Test (Word25)
structure Z = Test (Word30)
structure Z = Test (Word31)
structure Z = Test (Word32)
structure Z = Test (Word64)
