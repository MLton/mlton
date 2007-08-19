(* Copyright (C) 2003-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonFinalizable: MLTON_FINALIZABLE =
struct

structure List =
   struct
      open List

      fun push (l, x) = l := x :: !l

      fun foreach (l, f) = app f l
   end

datatype 'a t = T of {afters: (unit -> unit) list ref,
                      finalizers: ('a -> unit) list ref,
                      value: 'a ref}

fun touch (T {value, ...}) = Primitive.MLton.Finalizable.touch value

fun withValue (f as T {value, ...}, g) =
   DynamicWind.wind (fn () => g (!value),
                     fn () => touch f)

fun addFinalizer (T {finalizers, ...}, f) =
   List.push (finalizers, f)

val finalize =
   let
      val r: {clean: unit -> unit,
              isAlive: unit -> bool} list ref = ref []
      fun clean l =
         List.foldl (fn (z as {clean: unit -> unit, isAlive}, 
                         (gotOne, zs)) =>
                     if isAlive ()
                        then (gotOne, z :: zs)
                     else (clean (); (true, zs)))
         (false, []) l
      val _ = MLtonSignal.handleGC (fn () => r := #2 (clean (!r)))
      val _ =
         Cleaner.addNew
         (Cleaner.atExit, fn () =>
          let
             val l = !r
             (* Must clear r so that the handler doesn't interfere and so that
              * all other references to the finalizers are dropped.
              *)
             val _ = r := []
             fun loop l =
                let
                   val _ = MLtonGC.collect ()
                   val (gotOne, l) = clean l
                in
                   if gotOne
                      then loop l
                   else ()
                end
          in
             loop l
          end)
   in
      fn z => r := z :: !r
   end

fun new (v: 'a): 'a t =
   let
      val afters = ref []
      val finalizers = ref []
      val value = ref v
      val f = T {afters = afters,
                 finalizers = finalizers,
                 value = value}
      val weak = MLtonWeak.new value
      fun clean () =
         (List.foreach (!finalizers, fn f => f v)
          ; List.foreach (!afters, fn f => f ()))
      fun isAlive () = isSome (MLtonWeak.get weak)
      val _ = finalize {clean = clean, isAlive = isAlive}
   in
      f
   end

fun finalizeBefore (T {afters, ...}, f) =
   List.push (afters, fn () => touch f)

end
