functor Test (I: INTEGER) =
   struct
      fun foreach (l, f) = List.app f l

      val m = concat ["Int", Int.toString (valOf I.precision)]
         
      val _ = print (concat ["Testing ", m, "\n"])
         
      val nums =
         [valOf I.maxInt,
          I.- (valOf I.maxInt, I.fromInt 1)]
         @ (List.foldl
            (fn (i, ac) =>
             case SOME (I.fromInt i) handle Overflow => NONE of
                NONE => ac
              | SOME i => i :: ac)
            []
            [100, 10, 5, 2, 1, 0, ~1, ~2, ~5, ~10, ~100])
         @ [I.+ (I.fromInt 1, valOf I.minInt),
            valOf I.minInt]

      fun err msg = print (concat [m, ": ", concat msg, "\n"])

      datatype z = datatype StringCvt.radix
      val _ =
         foreach
         (nums, fn i =>
          foreach
          ([("toString", I.toString, LargeInt.toString),
            ("fmt BIN", I.fmt BIN, LargeInt.fmt BIN),
            ("fmt OCT", I.fmt OCT, LargeInt.fmt OCT),
            ("fmt DEC", I.fmt DEC, LargeInt.fmt DEC),
            ("fmt HEX", I.fmt HEX, LargeInt.fmt HEX)],
           fn (name, f, f') =>
           let
              val s = f i
              val s' = f' (I.toLarge i) handle Overflow => "Overflow"
           in
              if s = s'
                 then ()
              else err [name, " ", s, " <> ", name, " ", s']
           end))

      structure Answer =
         struct
            datatype t =
               Div
             | Int of I.int
             | Overflow

            val toString =
               fn Div => "Div"
                | Int i => I.toString i
                | Overflow => "Overflow"

            fun run (f: unit -> I.int): t =
               Int (f ())
               handle General.Div => Div
                    | General.Overflow => Overflow

            val equals: t * t -> bool = op =
         end

      val _ =
         foreach
         (nums, fn i =>
          let
             val a1 = Answer.Int i
             val a2 = Answer.run (fn () => I.fromLarge (I.toLarge i))
          in
             if Answer.equals (a1, a2)
                then ()
             else err ["fromLarge (toLarge ", I.toString i, ") = ",
                       Answer.toString a2]
          end)

      val _ =
         foreach
         ([("abs", I.abs, LargeInt.abs),
           ("~", I.~, LargeInt.~),
           ("fromString o toString",
            valOf o I.fromString o I.toString,
            valOf o LargeInt.fromString o LargeInt.toString)],
          fn (name, f, f') =>
          foreach
          (nums, fn i =>
           let
              val a = Answer.run (fn () => f i)
              val a' = Answer.run (fn () => I.fromLarge (f' (I.toLarge i)))
           in
              if Answer.equals (a, a')
                 then ()
              else err [name, " ", I.toString i,
                        " = ", Answer.toString a,
                        " <> ", Answer.toString a']
           end))

      val _ =
         foreach
         (nums, fn i =>
          foreach
          ([("BIN", BIN), ("OCT", OCT), ("DEC", DEC), ("HEX", HEX)],
           fn (rName, r) =>
           let
              val i' = valOf (StringCvt.scanString (I.scan r) (I.fmt r i))
           in
              if i = i'
                 then ()
              else err ["scan ", rName, " ", I.toString i, " = ", I.toString i']
           end))

      val _ =
         foreach
         ([("sign", I.sign, LargeInt.sign),
           ("toInt", I.toInt, LargeInt.toInt)],
          fn (name, f, f') =>
          foreach
          (nums, fn i =>
           let
              val a = Answer.run (fn () => I.fromInt (f i))
              val a' = Answer.run (fn () => I.fromInt (f' (I.toLarge i)))
           in
              if Answer.equals (a, a')
                 then ()
              else err [name, " ", I.toString i,
                        " = ", Answer.toString a,
                        " <> ", Answer.toString a']
           end))
         
      val _ =
         foreach
         ([("+", I.+, LargeInt.+),
           ("-", I.-, LargeInt.-),
           ("*", I.*, LargeInt.* ),
           ("div", I.div, LargeInt.div),
           ("max", I.max, LargeInt.max),
           ("min", I.min, LargeInt.min),
           ("mod", I.mod, LargeInt.mod),
           ("quot", I.quot, LargeInt.quot),
           ("rem", I.rem, LargeInt.rem)],
          fn (name,
              f: I.int * I.int -> I.int,
              f': LargeInt.int * LargeInt.int -> LargeInt.int) =>
          foreach
          (nums, fn i: I.int =>
           foreach
           (nums, fn j: I.int =>
            let
               val a = Answer.run (fn () => f (i, j))
               val a' = Answer.run (fn () =>
                                    I.fromLarge (f' (I.toLarge i, I.toLarge j)))
            in
               if Answer.equals (a, a')
                  then ()
               else err [I.toString i, " ", name, " ", I.toString j,
                         " = ", Answer.toString a, " <> ", Answer.toString a']
            end)))

      val _ =
         foreach
         ([(">", I.>, LargeInt.>),
           (">=", I.>=, LargeInt.>=),
           ("<", I.<, LargeInt.<),
           ("<=", I.<=, LargeInt.<=),
           ("sameSign", I.sameSign, LargeInt.sameSign)],
          fn (name, f, f') =>
          foreach
          (nums, fn i: I.int =>
           foreach
           (nums, fn j: I.int =>
            let
               val b = f (i, j)
               val b' = f' (I.toLarge i, I.toLarge j)
            in
               if b = b'
                  then ()
               else err [I.toString i, " ", name, " ", I.toString j,
                         " = ", Bool.toString b, " <> ", Bool.toString b']
            end)))

      structure Order =
         struct
            datatype t = datatype order

            val equals: t * t -> bool = op =

            val toString =
               fn EQUAL => "EQUAL"
                | GREATER => "GREATER"
                | LESS => "LESS"
         end
      
      val _ =
         foreach
         (nums, fn i =>
          foreach
          (nums, fn j =>
           let
              val ord = I.compare (i, j)
              val ord' = LargeInt.compare (I.toLarge i, I.toLarge j)
           in
              if Order.equals (ord, ord')
                 then ()
              else err ["compare (", I.toString i, ", ",
                        I.toString j, ") = ",
                        Order.toString ord,
                        " <> ",
                        Order.toString ord']
           end))
                 
   end

structure S = Test (Int2)
structure S = Test (Int3)
structure S = Test (Int4)
structure S = Test (Int7)
structure S = Test (Int8)
structure S = Test (Int9)
structure S = Test (Int13)
structure S = Test (Int16)
structure S = Test (Int17)
structure S = Test (Int20)
structure S = Test (Int25)
structure S = Test (Int30)
structure S = Test (Int31)
structure S = Test (Int32)
structure S = Test (Int64)
