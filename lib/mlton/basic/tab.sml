(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Tab: TAB =
struct

val initialSize: int = 40
val growFactor: int = 2

fun make(out, tab) =
   let
      fun makeTabs size =
         Array.tabulate(size,
                    let val prev = ref ""
                    in fn _ =>
                       (let val cur = !prev
                        in (prev := tab ^ cur ;
                            cur)
                        end)
                    end)

      val tabs = ref(makeTabs initialSize)

      fun size() = Array.length(!tabs)

      fun resize() = tabs := makeTabs(growFactor * size())

      val index = ref 0

      fun maybeResize() = if !index = size() then resize() else ()

      fun reset() = index := 0

      fun indent() = Out.output(out, Array.sub(!tabs, !index))

      fun right() = (index := !index + 1 ;
                     maybeResize())

      fun left() = if !index = 0 then Error.bug "Tab.left"
                   else index := !index - 1

      fun output x =
         (indent() ;
          Out.output(out, x) ;
          Out.output(out, "\n"))

   in {reset = reset,
       indent = indent,
       left = left,
       right = right}
   end

end
