(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Array2 (* : ARRAY2 *) =
   struct

      val op +? = SeqIndex.+?
      val op + = SeqIndex.+
      val op -? = SeqIndex.-?
      val op - = SeqIndex.-
      val op *? = SeqIndex.*?
      val op * = SeqIndex.*
      val op < = SeqIndex.<
      val op <= = SeqIndex.<=
      val op > = SeqIndex.>
      val op >= = SeqIndex.>=
      val ltu = SeqIndex.ltu
      val leu = SeqIndex.leu
      val gtu = SeqIndex.gtu
      val geu = SeqIndex.geu

      type 'a array = {array: 'a Array.array,
                       rows: SeqIndex.int,
                       cols: SeqIndex.int}

      fun dimensions' ({rows, cols, ...}: 'a array) = (rows, cols)
      fun dimensions ({rows, cols, ...}: 'a array) = 
         (SeqIndex.toIntUnsafe rows, SeqIndex.toIntUnsafe cols)
      fun nRows' ({rows, ...}: 'a array) = rows
      fun nRows ({rows, ...}: 'a array) = SeqIndex.toIntUnsafe rows
      fun nCols' ({cols, ...}: 'a array) = cols
      fun nCols ({cols, ...}: 'a array) = SeqIndex.toIntUnsafe cols

      type 'a region = {base: 'a array,
                        row: int,
                        col: int,
                        nrows: int option,
                        ncols: int option}

      local
         fun checkSliceMax' (start: int,
                             num: SeqIndex.int option,
                             max: SeqIndex.int): SeqIndex.int * SeqIndex.int =
            case num of
               NONE => if Primitive.Controls.safe
                          then let
                                  val start = 
                                     (SeqIndex.fromInt start)
                                     handle Overflow => raise Subscript
                               in
                                  if gtu (start, max)
                                     then raise Subscript
                                     else (start, max)
                               end
                          else (SeqIndex.fromIntUnsafe start, max)
             | SOME num => if Primitive.Controls.safe
                              then let
                                      val start = 
                                         (SeqIndex.fromInt start)
                                         handle Overflow => raise Subscript
                                   in
                                      if (start < 0 orelse num < 0
                                          orelse start +? num > max)
                                         then raise Subscript
                                         else (start, start +? num)
                                   end
                              else (SeqIndex.fromIntUnsafe start, 
                                    SeqIndex.fromIntUnsafe start +? num)
         fun checkSliceMax (start: int, 
                            num: int option, 
                            max: SeqIndex.int): SeqIndex.int * SeqIndex.int =
            if Primitive.Controls.safe
               then (checkSliceMax' (start, Option.map SeqIndex.fromInt num, max))
                    handle Overflow => raise Subscript
               else checkSliceMax' (start, Option.map SeqIndex.fromIntUnsafe num, max)
      in
         fun checkRegion' {base, row, col, nrows, ncols} =
            let 
               val (rows, cols) = dimensions' base
               val (startRow, stopRow) = checkSliceMax' (row, nrows, rows)
               val (startCol, stopCol) = checkSliceMax' (col, ncols, cols)
            in 
               {startRow = startRow, stopRow = stopRow,
                startCol = startCol, stopCol = stopCol}
            end
         fun checkRegion {base, row, col, nrows, ncols} =
            let 
               val (rows, cols) = dimensions' base
               val (startRow, stopRow) = checkSliceMax (row, nrows, rows)
               val (startCol, stopCol) = checkSliceMax (col, ncols, cols)
            in 
               {startRow = startRow, stopRow = stopRow,
                startCol = startCol, stopCol = stopCol}
            end
      end

      fun wholeRegion (a : 'a array): 'a region =
         {base = a, row = 0, col = 0, nrows = NONE, ncols = NONE}

      datatype traversal = RowMajor | ColMajor

      local
         fun make (rows, cols, doit) =
            if Primitive.Controls.safe 
               andalso (rows < 0 orelse cols < 0)
               then raise Size
            else {array = doit (rows * cols handle Overflow => raise Size),
                  rows = rows,
                  cols = cols}
      in
         fun arrayUninit' (rows, cols) =
            make (rows, cols, Array.arrayUninit')
         fun array' (rows, cols, init) =
            make (rows, cols, fn size => Array.array' (size, init))
      end
      local
         fun make (rows, cols, doit) =
            if Primitive.Controls.safe
               then let
                       val rows = 
                          (SeqIndex.fromInt rows)
                          handle Overflow => raise Size
                       val cols = 
                          (SeqIndex.fromInt cols)
                          handle Overflow => raise Size
                    in
                       doit (rows, cols)
                    end
               else doit (SeqIndex.fromIntUnsafe rows,
                          SeqIndex.fromIntUnsafe cols)
      in
         fun arrayUninit (rows, cols) =
            make (rows, cols, fn (rows, cols) => arrayUninit' (rows, cols))
         fun array (rows, cols, init) =
            make (rows, cols, fn (rows, cols) => array' (rows, cols, init))
      end

      fun array0 (): 'a array =
         {array = Array.arrayUninit' 0,
          rows = 0,
          cols = 0}

      fun unsafeSpot' ({cols, ...}: 'a array, r, c) =
         r *? cols +? c
      fun spot' (a as {rows, cols, ...}: 'a array, r, c) =
         if Primitive.Controls.safe 
            andalso (geu (r, rows) orelse geu (c, cols))
            then raise Subscript
            else unsafeSpot' (a, r, c)

      fun unsafeSub' (a as {array, ...}: 'a array, r, c) =
         Array.unsafeSub' (array, unsafeSpot' (a, r, c))
      fun sub' (a as {array, ...}: 'a array, r, c) =
         Array.unsafeSub' (array, spot' (a, r, c))
      fun unsafeUpdate' (a as {array, ...}: 'a array, r, c, x) =
         Array.unsafeUpdate' (array, unsafeSpot' (a, r, c), x)
      fun update' (a as {array, ...}: 'a array, r, c, x) =
         Array.unsafeUpdate' (array, spot' (a, r, c), x)

      local
         fun make (r, c, doit) =
            if Primitive.Controls.safe
               then let
                       val r = 
                          (SeqIndex.fromInt r)
                          handle Overflow => raise Subscript
                       val c = 
                          (SeqIndex.fromInt c)
                          handle Overflow => raise Subscript
                    in
                       doit (r, c)
                    end
               else doit (SeqIndex.fromIntUnsafe r,
                          SeqIndex.fromIntUnsafe c)
      in
         fun sub (a, r, c) =
            make (r, c, fn (r, c) => sub' (a, r, c))
         fun update (a, r, c, x) =
            make (r, c, fn (r, c) => update' (a, r, c, x))
      end

      fun 'a fromList (rows: 'a list list): 'a array =
         case rows of
            [] => array0 ()
          | row1 :: _ =>
               let
                  val cols = length row1
                  val a as {array, cols = cols', ...} = 
                     arrayUninit (length rows, cols)
                  val _ =
                     List.foldl
                     (fn (row: 'a list, i) =>
                      let
                         val max = i +? cols'
                         val i' =
                            List.foldl (fn (x: 'a, i) =>
                                        (if i >= max
                                            then raise Size
                                         else (Array.unsafeUpdate' (array, i, x)
                                               ; i +? 1)))
                            i row
                      in if i' = max
                            then i'
                         else raise Size
                      end)
                     0 rows
               in
                  a
               end

      fun row' ({array, rows, cols}, r) =
         if Primitive.Controls.safe andalso geu (r, rows)
            then raise Subscript
         else
            ArraySlice.vector (ArraySlice.slice' (array, r *? cols, SOME cols))
      fun row (a, r) =
         if Primitive.Controls.safe
            then let
                    val r = 
                       (SeqIndex.fromInt r)
                       handle Overflow => raise Subscript
                 in
                    row' (a, r)
                 end
            else row' (a, SeqIndex.fromIntUnsafe r)
      fun column' (a as {rows, cols, ...}: 'a array, c) =
         if Primitive.Controls.safe andalso geu (c, cols)
            then raise Subscript
         else
            Vector.tabulate' (rows, fn r => unsafeSub' (a, r, c))
      fun column (a, c) =
         if Primitive.Controls.safe
            then let
                    val c = 
                       (SeqIndex.fromInt c)
                       handle Overflow => raise Subscript
                 in
                    column' (a, c)
                 end
            else column' (a, SeqIndex.fromIntUnsafe c)

      fun foldi' trv f b (region as {base, ...}) =
         let
            val {startRow, stopRow, startCol, stopCol} = checkRegion region
         in
            case trv of
               RowMajor =>
                  let
                     fun loopRow (r, b) =
                        if r >= stopRow then b
                           else let
                                   fun loopCol (c, b) =
                                      if c >= stopCol then b
                                         else loopCol (c +? 1, f (r, c, sub' (base, r, c), b))
                                in
                                   loopRow (r +? 1, loopCol (startCol, b))
                                end
                  in
                     loopRow (startRow, b)
                  end
             | ColMajor =>
                  let
                     fun loopCol (c, b) =
                        if c >= stopCol then b
                           else let
                                   fun loopRow (r, b) =
                                      if r >= stopRow then b
                                         else loopRow (r +? 1, f (r, c, sub' (base, r, c), b))
                                in
                                   loopCol (c +? 1, loopRow (startRow, b))
                                end
                  in
                     loopCol (startCol, b)
                  end
         end

      fun foldi trv f b a =
         foldi' trv (fn (r, c, x, b) => 
                     f (SeqIndex.toIntUnsafe r, 
                        SeqIndex.toIntUnsafe c, 
                        x, b)) b a
      fun fold trv f b a =
          foldi trv (fn (_, _, x, b) => f (x, b)) b (wholeRegion a)

      fun appi trv f =
         foldi trv (fn (r, c, x, ()) => f (r, c, x)) ()

      fun app trv f = fold trv (f o #1) ()

      fun modifyi trv f (r as {base, ...}) =
         appi trv (fn (r, c, x) => update (base, r, c, f (r, c, x))) r

      fun modify trv f a = modifyi trv (f o #3) (wholeRegion a)

      fun tabulate trv (rows, cols, f) =
         let 
            val a = arrayUninit (rows, cols)
            val () = modifyi trv (fn (r, c, _) => f (r, c)) (wholeRegion a)
         in 
            a
         end

      fun copy {src = src as {base, ...}: 'a region,
                dst, dst_row, dst_col} =
         let
            val {startRow, stopRow, startCol, stopCol} = checkRegion src
            val nrows = stopRow -? startRow
            val ncols = stopCol -? startCol
            val {startRow = dst_row, startCol = dst_col, ...} = 
               checkRegion' {base = dst, row = dst_row, col = dst_col,
                             nrows = SOME nrows, 
                             ncols = SOME ncols}
            fun forUp (start, stop, f: SeqIndex.int -> unit) =
               let
                  fun loop i =
                     if i >= stop
                        then ()
                     else (f i; loop (i + 1))
               in loop start
               end
            fun forDown (start, stop, f: SeqIndex.int -> unit) =
               let
                  fun loop i =
                     if i < start
                        then ()
                     else (f i; loop (i - 1))
               in loop (stop -? 1)
               end
            val forRows = if startRow <= dst_row then forDown else forUp
            val forCols = if startCol <= dst_col then forUp else forDown
         in forRows (0, nrows, fn r =>
            forCols (0, ncols, fn c =>
                     unsafeUpdate' (dst, dst_row +? r, dst_col +? c,
                                    unsafeSub' (base, startRow +? r, startCol +? c))))
         end
   end
