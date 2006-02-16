(*
 * Written by sweeks@sweeks.com on 1999-08-31.
 *
 * A solution to mpuz. (Try M-x mpuz in emacs.)
 * This solution is very loosely based on an OCAML solution posted to
 * comp.lang.ml by Laurent Vaucher <blo.b@infonie.fr>.
 *)

(* override print so the benchmark is silent *)
fun print _ = ()

structure List =
   struct
      open List

      fun exists(l, p) = List.exists p l
         
      fun map(l, f) = List.map f l

      fun fold(l, b, f) =
         let
            fun loop(l, b) =
               case l of
                  [] => b
                | x :: l => loop(l, f(x, b))
         in loop(l, b)
         end

      fun foreach(l, f) = fold(l, (), fn (x, ()) => f x)
   end

structure String =
   struct
      open String

      fun fold(s, b, f) =
         let
            val n = size s
            fun loop(i, b) =
               if i = n
                  then b
               else loop(i + 1, f(String.sub(s, i), b))
         in loop(0, b)
         end
   end

structure Mpuz =
   struct
      fun solve(a, b, c, d, e) =
         let
            fun printNewline() = print "\n"
            val sub = Array.sub
            val update = Array.update

            val letters =
               List.fold
               ([a, b, c, d, e], [], fn (s, letters) =>
                String.fold
                (s, letters, fn (c, letters) =>
                 if List.exists(letters, fn c' => c = c')
                    then letters
                 else c :: letters))

            val letterValues =
               Array.array(Char.ord Char.maxChar + 1, 0)

            fun letterValue(c) =
               Array.sub(letterValues, ord c)

            fun setLetterValue(c, v) =
               Array.update(letterValues, ord c, v)

            fun stringValue(s) =
               String.fold(s, 0, fn (c, v) => v * 10 + letterValue c)

            fun printResult() =
               (List.foreach
                (letters, fn c =>
                 print(concat[String.str(c), " = ",
                              Int.toString(letterValue(c)), " "]))
                ; print "\n")

            fun testOk() =
               let
                  val b0 = letterValue(String.sub(b, 1))
                  val b1 = letterValue(String.sub(b, 0))
                  val a = stringValue a
                  val b = stringValue b
                  val c = stringValue c
                  val d = stringValue d
                  val e = stringValue e
               in if a * b0 = c
                     andalso a * b1 = d
                     andalso a * b = e
                     andalso c + d * 10 = e
                     then printResult()
                  else ()
               end

            val values = List.map([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], fn v =>
                                  (v, ref false))

            (* Try all assignments of values to letters. *)
            fun loop(letters) =
               case letters of
                  [] => testOk()
                | c :: letters =>
                     List.foreach
                     (values, fn (v, r) =>
                      if !r
                         then ()
                      else (r := true
                            ; setLetterValue(c, v)
                            ; loop(letters)
                            ; r := false))

         in loop(letters)
         end
   end

structure Main =
   struct
      fun doit() =
         Mpuz.solve("AGH", "FB", "CBEE", "GHFD", "FGIJE")
      (*
       * Solution:
       * J = 0 I = 1 D = 8 E = 2 C = 5 B = 6 F = 4 H = 7 G = 3 A = 9
       *)

      val doit =
         fn size =>
         let
            fun loop n =
               if n = 0
                  then ()
               else (doit();
                     loop(n-1))
         in
            loop size
         end
   end
