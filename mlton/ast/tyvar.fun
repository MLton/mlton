(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Tyvar (S: TYVAR_STRUCTS): TYVAR = 
struct

open S

structure Wrap = Region.Wrap
open Wrap
type node' = {name: string,
              equality: bool,
              hash: Word.t,
              plist: PropertyList.t}
type t = node' Wrap.t
type obj = t

fun toString (tyvar: t) =
   let val {name, equality, ...} = node tyvar
   in (if equality then "''" else "'") ^ name
   end

val layout = Layout.str o toString

local
   fun make sel (tyvar:t) = sel (node tyvar)
in
   val name = make #name
   val hash = make #hash
   val plist = make #plist
   val isEquality = make #equality
end

val clear = PropertyList.clear o plist
fun equals (a, a') = PropertyList.equals (plist a, plist a')
fun sameName (a, a') = String.equals (name a, name a')

fun newRegion ({name, equality}, region) =
   makeRegion ({name = name,
                equality = equality,
                hash = Random.word (),
                plist = PropertyList.new ()},
               region)

fun new f = newRegion (f, Region.bogus)

fun newLike a = newRegion ({equality = isEquality a,
                            name = name a},
                           region a)

fun newString (s, {left, right}) =
   newRegion (if String.size s > 1
                 andalso Char.equals (#"'", String.sub (s, 1))
                 then {name = String.dropPrefix (s, 2),
                       equality = true}
              else {name = String.dropPrefix (s, 1),
                    equality = false},
              Region.make {left = left, right = right})

(*val make = Trace.trace2 ("Tyvar.make", String.layout, Bool.layout,
 *                      layout) make
 *)

local val c = Counter.new 0
in 
   fun reset () = Counter.reset (c, 0)
   (* quell unused warning *)
   val _ = reset
   fun newNoname {equality} =
      new {name = "a_" ^ Int.toString (Counter.next c),
           equality = equality}
end

local open Layout
in
   fun layouts ts =
      case Vector.length ts of
         0 => empty
       | 1 => layout (Vector.sub (ts, 0))
       | _ => Vector.layout layout ts
end

end
