structure COld =
   struct
      open Int
         
      fun makeLength (sub, term) p =
         let
            fun loop i =
               if term (sub (p, i))
                  then i
               else loop (i +? 1)
         in loop 0
         end

      fun toArrayOfLength (s: 'a,
                           sub: 'a * int -> 'b,
                           n: int) : 'b array =
         let
            val a = Primitive.Array.arrayUnsafe n
            fun loop i =
               if i >= n
                  then ()
               else (Array.update (a, i, sub (s, i))
                     ; loop (i + 1))
         in loop 0;
            a
         end

      structure CS =
         struct
            type t = Primitive.MLton.Pointer.t

            fun sub (cs, i) =
               Primitive.Char8.fromWord8Unsafe (Primitive.MLton.Pointer.getWord8 (cs, i))

            fun update (cs, i, c) =
               Primitive.MLton.Pointer.setWord8 (cs, i, Primitive.Char8.toWord8Unsafe c)

            fun toCharArrayOfLength (cs, n) = toArrayOfLength (cs, sub, n)

            fun toStringOfLength cs =
               String.fromArray (CharArray.fromPoly (toCharArrayOfLength cs))

            val length = makeLength (sub, fn #"\000" => true | _ => false)

            fun toString cs = toStringOfLength (cs, length cs)
         end
      
   end

structure MLtonCallStack =
   struct
      open Primitive.MLton.CallStack

      val gcState = Primitive.MLton.GCState.gcState
      structure Pointer = Primitive.MLton.Pointer
         
      val current: unit -> t =
         fn () =>
         if not keep
            then T (Array.array (0, 0w0))
         else
            let
               val a = Array.array (Word32.toInt (numStackFrames gcState), 0w0)
               val () = callStack (gcState, a)
            in
               T a
            end

      val toStrings: t -> string list =
         fn T a =>
         if not keep
            then []
         else
            let
               val skip = Array.length a - 2
            in
               Array.foldri
               (fn (i, frameIndex, ac) =>
                if i >= skip
                   then ac
                else
                   let
                      val p = frameIndexSourceSeq (gcState, frameIndex)
                      val max = Pointer.getInt32 (p, 0)
                      fun loop (j, ac) =
                         if j > max
                            then ac
                         else loop (j + 1,
                                    COld.CS.toString (sourceName
                                                      (gcState, Pointer.getWord32 (p, j)))
                                    :: ac)
                   in
                      loop (1, ac)
                   end)
               [] a
            end
   end

structure MLtonExn =
   struct
      open Primitive.MLton.Exn

      type t = exn
         
      val addExnMessager = General.addExnMessager

      val history: t -> string list =
         if keepHistory then
            (setInitExtra (NONE: extra)
             ; setExtendExtra (fn e =>
                               case e of
                                  NONE => SOME (MLtonCallStack.current ())
                                | SOME _ => e)
             ; (fn e =>
                case extra e of
                   NONE => []
                 | SOME cs =>
                      let
                         (* Gets rid of the anonymous function passed to
                          * setExtendExtra above.
                          *)
                         fun loop xs =
                            case xs of
                               [] => []
                             | x :: xs =>
                                  if String.isPrefix "MLtonExn.fn " x then
                                     xs
                                  else
                                     loop xs
                      in
                         loop (MLtonCallStack.toStrings cs)
                      end))
         else fn _ => []

      local
         val message = PrimitiveFFI.Stdio.print
      in
         fun 'a topLevelHandler (exn: exn): 'a =
            (message (concat ["unhandled exception: ", exnMessage exn, "\n"])
             ; (case history exn of
                   [] => ()
                 | l =>
                      (message "with history:\n"
                       ; (List.app (fn s => message (concat ["\t", s, "\n"]))
                          l)))
             ; Primitive.MLton.bug (Primitive.NullString8.fromString 
                                    "unhandled exception in Basis Library\000")
             ; raise Fail "bug")
            handle _ => (message "Toplevel handler raised exception.\n"
                         ; Primitive.MLton.bug (Primitive.NullString8.fromString 
                                                "unhandled exception in Basis Library\000")
                         (* The following raise is unreachable, but must be there
                          * so that the expression is of type 'a.
                          *)
                         ; raise Fail "bug")
      end
   end

val _ = 
   Primitive.TopLevel.setHandler MLtonExn.topLevelHandler
