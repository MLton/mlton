(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Array2: ARRAY2 =
   struct
      open Primitive.Int

      (* I am careful to use a type here instead of a datatype so that
       * 'a array will be an equality type irrespective of whether 'a is.
       * This is probably just an NJ-ism, but I don't want to think about it.
       *)
      type 'a array = {rows: int,
                       cols: int,
                       array: 'a Array.array}

      fun dimensions ({rows, cols, ...}: 'a array) = (rows, cols)
      fun nRows ({rows, ...}: 'a array) = rows
      fun nCols ({cols, ...}: 'a array) = cols

      type 'a region = {base: 'a array,
                        row: int,
                        col: int,
                        nrows: int option,
                        ncols: int option}

      fun checkSliceMax (start: int, num: int option, max: int): int =
         case num of
            NONE =>
               if Primitive.safe andalso (start < 0 orelse start > max) then
                  raise Subscript
               else
                  max
          | SOME num =>
               if Primitive.safe
                  andalso (start < 0
                           orelse num < 0
                           orelse start > max -? num) then
                  raise Subscript
               else
                  start +? num

      fun checkRegion {base, row, col, nrows, ncols} =
         let
            val (rows, cols) = dimensions base
         in
            {stopRow = checkSliceMax (row, nrows, rows),
             stopCol = checkSliceMax (col, ncols, cols)}
         end
      
      fun wholeRegion (a: 'a array): 'a region =
         {base = a, row = 0, col = 0, nrows = NONE, ncols = NONE}

      datatype traversal = RowMajor | ColMajor

      local
         fun make (rows, cols, doit) =
            if Primitive.safe andalso (rows < 0 orelse cols < 0)
               then raise Size
            else {rows = rows,
                  cols = cols,
                  array = doit (rows * cols handle Overflow => raise Size)}
      in
         fun arrayUninit (rows, cols) =
            make (rows, cols, Primitive.Array.array)
         fun array (rows, cols, init) =
            make (rows, cols, fn size => Array.array (size, init))
      end

      fun array0 (): 'a array =
         {rows = 0,
          cols = 0,
          array = Primitive.Array.array 0}

      fun spot ({rows, cols, ...}: 'a array, r, c) =
         if Primitive.safe andalso (geu (r, rows) orelse geu (c, cols))
            then raise Subscript
         else r *? cols +? c
         
      fun sub (a as {array, ...}: 'a array, r, c) =
         Primitive.Array.sub (array, spot (a, r, c))

      fun update (a as {array, ...}: 'a array, r, c, x) =
         Primitive.Array.update (array, spot (a, r, c), x)

      fun 'a fromList (rows: 'a list list): 'a array =
         case rows of
            [] => array0 ()
          | row1 :: _ =>
               let
                  val cols = length row1
                  val a as {array, ...} = arrayUninit (length rows, cols)
                  val _ =
                     List.foldl
                     (fn (row: 'a list, i) =>
                      let
                         val max = i +? cols
                         val i' =
                            List.foldl (fn (x: 'a, i) =>
                                        (if i >= max
                                            then raise Size
                                         else (Primitive.Array.update (array, i, x)
                                               ; i + 1)))
                            i row
                      in if i' = max
                            then i'
                         else raise Size
                      end)
                     0 rows
               in
                  a
               end

      fun row ({rows, cols, array}, r) =
         if Primitive.safe andalso geu (r, rows)
            then raise Subscript
         else
            ArraySlice.vector (ArraySlice.slice (array, r *? cols, SOME cols))

      fun column (a as {rows, cols, ...}: 'a array, c) =
         if Primitive.safe andalso geu (c, cols)
            then raise Subscript
         else
            Vector.tabulate (rows, fn r => sub(a, r, c))

      fun foldi trv f b (region as {base, row, col, ...}) =
         let
            val {stopRow, stopCol} = checkRegion region
         in
            case trv of
               RowMajor =>
                  Util.naturalFoldStartStop
                  (row, stopRow, b, fn (r, b) =>
                   Util.naturalFoldStartStop
                   (col, stopCol, b, fn (c, b) =>
                    f (r, c, sub (base, r, c), b)))
             | ColMajor =>
                  Util.naturalFoldStartStop
                  (col, stopCol, b, fn (c, b) =>
                   Util.naturalFoldStartStop
                   (row, stopRow, b, fn (r, b) =>
                    f (r, c, sub (base, r, c), b)))
         end

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

      fun copy {src = src as {base, row, col, ...}: 'a region,
                dst, dst_row, dst_col} =
         let
            val {stopRow, stopCol} = checkRegion src
            val nrows = stopRow -? row
            val ncols = stopCol -? col
            val _ = checkRegion {base = dst, row = dst_row, col = dst_col,
                                nrows = SOME nrows, ncols = SOME ncols}
            fun for (start, stop, f: int -> unit) =
               let
                  fun loop i =
                     if i >= stop
                        then ()
                     else (f i; loop (i + 1))
               in loop start
               end
            fun forDown (start, stop, f: int -> unit) =
               let
                  fun loop i =
                     if i < start
                        then ()
                     else (f i; loop (i - 1))
               in loop (stop -? 1)
               end
            val forRows = if row <= dst_row then forDown else for
            val forCols = if col <= dst_col then for else forDown
         in forRows (0, nrows, fn r =>
                     forCols (0, ncols, fn c =>
                              update (dst, dst_row +? r, dst_col +? c,
                                      sub (base, row +? r, col +? c))))
         end
   end
