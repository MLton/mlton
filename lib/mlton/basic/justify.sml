(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Justify: JUSTIFY =
struct

structure C = Char
structure S = String

datatype t =
    Left
  | Center
  | Right

val toString =
   fn Left => "Left"
    | Center => "Center"
    | Right => "Right"

val layout = Layout.str o toString

fun spaces n = S.make (n, C.space)

fun justify (s, width, just) =
    let val numchars = S.size s
        val numspaces = width - numchars
    in S.concat
       (case just of
           Left => [s, spaces numspaces]
         | Center => let val numLeft = numspaces div 2
                         val numRight = numspaces - numLeft
                     in [spaces numLeft, s, spaces numRight]
                     end
         | Right => [spaces numspaces, s])
    end

fun table {columnHeads: string list option,
           justs: t list,
           rows: string list list} =
   let
      val headsAndRows =
         case columnHeads of
            NONE => rows
          | SOME h => h :: rows
      val maxs =
         List.fold (headsAndRows,
                    List.revMap (justs, fn _ => 0),
                    fn (row, ms) =>
                    List.map2 (row, ms, fn (s, m) => Int.max (m, String.size s)))
      val rows =
         List.map (rows, fn row => List.map3 (row, maxs, justs, justify))
      val rows =
         case columnHeads of
            NONE => rows
          | SOME heads =>
               let
                  val heads = List.map2 (heads, maxs, fn (s, i) =>
                                         justify (s, i, Center))
                  val dashes = List.map (maxs, fn i => String.make (i, #"-"))
               in
                  heads :: dashes :: rows
               end
   in
      rows
   end

val table =
   Trace.trace ("Justify.table",
                fn {columnHeads, justs, rows} =>
                Layout.record [("columnHeads",
                                Option.layout (List.layout String.layout)
                                columnHeads),
                               ("justs", List.layout layout justs),
                               ("rows",
                                List.layout (List.layout String.layout) rows)],
                List.layout (List.layout String.layout))
   table

fun tableOfColumns (columns: (t * string list) list) =
   let
      val justs = List.map (columns, #1)
      val columns = List.map (columns, #2)
      fun loop (columns: string list list, ac: string list list) =
         if List.isEmpty (hd columns)
            then rev ac
         else loop (List.map (columns, tl), List.map (columns, hd) :: ac)
      val rows = loop (columns, [])
   in
      table {columnHeads = NONE,
             justs = justs,
             rows = rows}
   end

fun outputTable (t, out) =
   let
      val print = Out.outputc out
   in
      List.foreach (t, fn ss =>
                    (case ss of
                        [] => ()
                      | s :: ss =>
                           (print s
                            ; List.foreach (ss, fn s => (print " "; print s)))
                           ; print "\n"))
   end

end
