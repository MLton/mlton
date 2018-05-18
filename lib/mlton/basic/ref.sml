(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Ref: REF =
struct

type 'a t = 'a ref

val (op !) = (op !)

val (op :=) = op :=

fun equals (r: 'a t, r') = r = r'

fun swap (r, r') = let val v = !r
                  in r := !r'; r' := v
                  end

fun getAndSet sel = (! o sel, fn (x, v) => sel x := v)

fun ('a, 'b) fluidLet (r: 'a t, x: 'a, th: unit -> 'b): 'b =
   let
      val old = !r
      val () = r := x
   in
      Exn.finally (th, fn () => r := old)
   end

fun getSet layout = 
   let val r = ref NONE
      fun get () =
         case !r of
            NONE => Error.bug "Ref.getSet.get: not available"
          | SOME v => v
      fun set v = r := SOME v
      fun clear () = r := NONE
      val layout = fn () => layout (get ())
      fun output out = Layout.output (layout (), out)
      fun print () = output Out.standard
   in {get = get,
       set = set,
       clear = clear,
       layout = layout,
       output = output,
       print = print}
   end

fun layout layoutX r = layoutX (!r)

fun memoize (r: 'a option ref, f: unit -> 'a): 'a =
   case !r of
      NONE =>
         let
            val a = f ()
            val () = r := SOME a
         in
            a
         end
    | SOME a => a

end
