(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonCont:> MLTON_CONT =
struct

structure Thread = Primitive.MLton.Thread

fun die (s: string): 'a =
   (PrimitiveFFI.Stdio.print s
    ; PrimitiveFFI.Posix.Process.exit 1
    ; let exception DieFailed
      in raise DieFailed
      end)

val gcState = Primitive.MLton.GCState.gcState

type 'a t = (unit -> 'a) -> unit

fun callcc (f: 'a t -> 'a): 'a =
   if MLtonThread.amInSignalHandler ()
       then die "Cont.callcc can not be used in a signal handler\n"
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
                   val t = Thread.savedPre gcState
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


(* ********** *)
val isolate: ('a -> unit) -> 'a t =
   fn (f: 'a -> unit) =>
   callcc
   (fn k1 =>
    let
       val x = callcc (fn k2 => throw (k1, k2))
       val _ = (f x ; Exit.topLevelSuffix ())
               handle exn => MLtonExn.topLevelHandler exn
    in
       raise Fail "MLton.Cont.isolate: return from (wrapped) func"
    end)

(* ********** *)
local
val base: (unit -> unit) t =
   callcc
   (fn k1 =>
    let
       val th = callcc (fn k2 => throw (k1, k2))
       val _ = (th () ; Exit.topLevelSuffix ())
               handle exn => MLtonExn.topLevelHandler exn
    in
       raise Fail "MLton.Cont.isolate: return from (wrapped) func"
    end)
in
val isolate: ('a -> unit) -> 'a t =
   fn (f: 'a -> unit) =>
   callcc
   (fn k1 =>
    let
       val x = callcc (fn k2 => throw (k1, k2))
    in
       throw (base, fn () => f x)
    end)
end

(* ********** *)
local
val base: (unit -> unit) t =
   callcc
   (fn k1 =>
    let
       val th = callcc (fn k2 => throw (k1, k2))
       val _ = (th () ; Exit.topLevelSuffix ())
               handle exn => MLtonExn.topLevelHandler exn
    in
       raise Fail "MLton.Cont.isolate: return from (wrapped) func"
    end)
in
val isolate: ('a -> unit) -> 'a t =
   fn (f: 'a -> unit) =>
   callcc
   (fn k1 =>
    throw (base, fn () =>
           let
              val x = callcc (fn k2 => throw (k1, k2))
           in
              throw (base, fn () => f x)
           end))
end

(* ********** *)
local
val base: (unit -> unit) option t =
   let
      val base: (unit -> unit) option t option ref = ref NONE
      val th = callcc (fn k => (base := SOME k; NONE))
   in
      case th of
         NONE => (case !base of
                     NONE => raise Fail "MLton.Cont.isolate: missing base"
                   | SOME base => base)
       | SOME th => let
                       val _ = (th () ; Exit.topLevelSuffix ())
                               handle exn => MLtonExn.topLevelHandler exn
                    in
                       raise Fail "MLton.Cont.isolate: return from (wrapped) func"
                    end
   end
in
val isolate: ('a -> unit) -> 'a t =
   fn (f: 'a -> unit) =>
   callcc
   (fn k1 =>
    throw (base, SOME (fn () =>
           let
              val x = callcc (fn k2 => throw (k1, k2))
           in
              throw (base, SOME (fn () => f x))
           end)))
end

(* ********** *)
local
val base: (unit -> unit) option t =
   let
      val base: (unit -> unit) option t ref =
         ref (fn _ => raise Fail "MLton.Cont.isolate: missing base")
      val th = callcc (fn k => (base := k; NONE))
   in
      case th of
         NONE => !base
       | SOME th => let
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
   throw (base, SOME (f o v))
end

(* ********** *)
local
val thunk: (unit -> unit) option ref = ref NONE
val base: unit t =
   let
      val base: unit t ref =
         ref (fn _ => raise Fail "MLton.Cont.isolate: missing base")
      val () = callcc (fn k => base := k)
   in
      case !thunk of
         NONE => !base
       | SOME th =>
            let
               val _ = thunk := NONE
               val _ = Thread.atomicEnd () (* Match 1 *)
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
      val () = thunk := SOME (f o v)
   in
      throw (base, ())
   end
end

(* ********** *)
local
val thunk: (unit -> unit) option ref = ref NONE
val base: Thread.preThread =
   let
      val () = Thread.copyCurrent ()
   in
      case !thunk of
         NONE => Thread.savedPre gcState
       | SOME th =>
            let
               val () = thunk := NONE
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
      val () = thunk := SOME (f o v)
      val new = Thread.copy base
   in
      Thread.switchTo new (* Match 2 *)
   end
end

end
