(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure List: LIST =
  struct
     open Int

     datatype list = datatype Primitive.List.list

     exception Empty

     val null =
        fn [] => true
         | _ => false

     val hd =
        fn x :: _ => x
         | _ => raise Empty

     val tl =
        fn _ :: l => l
         | _ => raise Empty

     val rec last =
        fn [] => raise Empty
         | [x] => x
         | _ :: l => last l

     val getItem =
        fn [] => NONE
         | x :: r => SOME (x, r)

     fun foldl f b l =
        let
           fun loop (l, b) =
              case l of
                 [] => b
               | x :: l => loop (l, f (x, b))
        in loop (l, b)
        end

     fun length l = foldl (fn (_, n) => n +? 1) 0 l

     fun appendRev (l1, l2) = foldl (op ::) l2 l1

     val revAppend = appendRev

     fun rev l = appendRev (l, [])

     fun l1 @ l2 =
        case l2 of
           [] => l1
         | _ => appendRev (rev l1, l2)

     fun foldr f b l = foldl f b (rev l)

     fun concat ls = foldr (op @) [] ls

     fun app f = foldl (f o #1) ()

     fun map f l = rev (foldl (fn (x, l) => f x :: l) [] l)

     fun mapPartial pred l =
        rev (foldl (fn (x, l) => (case pred x of
                                   NONE => l
                                 | SOME y => y :: l))
                   [] l)

     fun filter pred = mapPartial (fn x => if pred x then SOME x else NONE)

     fun partition pred l =
        let
           val (pos, neg) =
              foldl (fn (x, (trues, falses)) =>
                     if pred x then (x :: trues, falses)
                     else (trues, x :: falses))
              ([], []) l
        in (rev pos, rev neg)
        end

     fun find pred =
        let
           val rec loop =
              fn [] => NONE
               | x :: l => if pred x
                            then SOME x
                         else loop l
        in loop
        end

     fun exists pred l =
        case find pred l of
           NONE => false
         | SOME _ => true

     fun all pred = not o (exists (not o pred))

     fun tabulate (n, f) = 
        if Primitive.Controls.safe andalso n < 0
           then raise Size
        else let
                fun loop (i, ac) =
                   if i < n
                      then loop (i + 1, f i :: ac)
                   else rev ac
             in loop (0, [])
             end

     fun nth (l, n) =
        let
           fun loop (l, n) =
              case l of
                 [] => raise Subscript
               | x :: l =>
                    if n > 0
                       then loop (l, n - 1)
                    else x
        in
           if Primitive.Controls.safe andalso n < 0
              then raise Subscript
           else loop (l, n)
        end

     fun take (l, n) =
        let
           fun loop (l, n, ac) =
              if n > 0
                 then (case l of
                          [] => raise Subscript
                        | x :: l => loop (l, n - 1, x :: ac))
              else rev ac
        in
           if Primitive.Controls.safe andalso n < 0
              then raise Subscript
           else loop (l, n, [])
        end

     fun drop (l, n) =
        let
           fun loop (l, n) =
              if n > 0
                 then (case l of
                          [] => raise Subscript
                        | _ :: l => loop (l, n - 1))
              else l
        in
           if Primitive.Controls.safe andalso n < 0
              then raise Subscript
           else loop (l, n)
        end

     fun collate cmp =
        let
           val rec loop =
             fn ([], []) => EQUAL
              | ([], _) => LESS
              | (_, []) => GREATER
              | (x1::l1,x2::l2) => (case cmp (x1, x2) of
                                      EQUAL => loop (l1, l2)
                                    | ans => ans)
        in loop
        end
  end

structure ListGlobal: LIST_GLOBAL = List
open ListGlobal
