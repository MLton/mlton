(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonCont:> MLTON_CONT =
struct

structure Thread =
   struct
      open Primitive.MLton.Thread
      val savedPre = fn () => savedPre Primitive.MLton.GCState.gcState
   end

fun die (s: string): 'a =
   (PrimitiveFFI.Stdio.print s
    ; PrimitiveFFI.Posix.Process.exit 1
    ; let exception DieFailed
      in raise DieFailed
      end)

type 'a t = (unit -> 'a) -> unit

fun callcc (f: 'a t -> 'a): 'a =
   if MLtonThread.amInSignalHandler ()
       then die "MLton.Cont.callcc can not be used in a signal handler\n"
    else 
       let
          datatype 'a state =
             Original of 'a t -> 'a
           | Copy of unit -> 'a
           | Clear
          val r: 'a state ref = ref (Original f)
          val _ = Thread.atomicBegin () (* Match 1 *)
          val _ = Thread.copyCurrent ()
       in
          case (!r before r := Clear) of
             Clear => raise Fail "MLton.Cont.callcc: Clear"
           | Copy v =>
                let
                   val _ = Thread.atomicEnd () (* Match 2 *)
                in
                   v ()
                end
           | Original f =>
                let
                   val t = Thread.savedPre ()
                   val _ = Thread.atomicEnd () (* Match 1 *)
                in
                   f (fn v =>
                      let
                         val _ = Thread.atomicBegin () (* Match 2 *)
                         val _ = r := Copy v
                         val new = Thread.copy t
                         val _ = Thread.atomicBegin () (* Match 3 *)
                      in
                         Thread.switchTo new (* Match 3 *)
                      end)
                end
       end

fun ('a, 'b) throw' (k: 'a t, v: unit -> 'a): 'b =
   (k v; raise Fail "MLton.Cont.throw': return from continuation")

fun ('a, 'b) throw (k: 'a t, v: 'a): 'b = throw' (k, fn () => v)

fun prepend (k, f) v = throw' (k, f o v)

local
val thRef: (unit -> unit) option ref = ref NONE
val base: Thread.preThread =
   let
      val () = Thread.copyCurrent ()
   in
      case !thRef of
         NONE => Thread.savedPre ()
       | SOME th =>
            let
               val () = thRef := NONE
               val () = Thread.atomicEnd () (* Match 1 *)
               val _ = (th () ; Exit.topLevelSuffix ())
                       handle exn => MLtonExn.topLevelHandler exn
            in
               raise Fail "MLton.Cont.isolate: return from (wrapped) func"
            end
   end
in
val isolate: ('a -> unit) -> 'a t =
   fn (f: 'a -> unit) =>
   fn (v: unit -> 'a) =>
   let
      val _ = Thread.atomicBegin () (* Match 1 *)
      val _ = Thread.atomicBegin () (* Match 2 *)
      val () = thRef := SOME (f o v)
      val new = Thread.copy base
   in
      Thread.switchTo new (* Match 2 *)
   end
end

end
