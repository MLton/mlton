(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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

fun table {justs: t list,
	   rows: string list list} =
   let
      val maxs =
	 List.fold (rows, List.revMap (justs, fn _ => 0), fn (row, ms) =>
		    List.map2 (row, ms, fn (s, m) => Int.max (m, String.size s)))
   in List.map (rows, fn row => List.map3(row, maxs, justs, justify))
   end

val table =
   Trace.trace ("table",
		fn {justs, rows} =>
		Layout.record [("justs", List.layout layout justs),
			       ("rows",
				List.layout (List.layout String.layout) rows)],
		List.layout (List.layout String.layout))
   table

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
