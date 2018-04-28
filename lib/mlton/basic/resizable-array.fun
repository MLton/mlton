(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ResizableArray (): RESIZABLE_ARRAY =
struct

structure Array = Array

structure A' =
   struct
      datatype 'a t = T of {array: 'a option Array.t ref,
                            length: int ref}

      fun getArray (T {array, ...}) = !array
      fun lengthRef (T {length, ...}) = length
      fun length a = ! (lengthRef a)
      val shape = length

      fun maxLength a = Array.length (getArray a)
      fun minLength a = Int.quot (maxLength a, 4)

      fun invariant a =
         maxLength a >= 1
         andalso minLength a <= length a
         andalso length a <= maxLength a

      fun incLength a = Int.inc (lengthRef a)
      fun decLength a = Int.dec (lengthRef a)
      fun maxIndex a = length a - 1

      exception New = Array.New

      fun empty () =
         T {array = ref (Array.new (1, NONE)),
           length = ref 0}

      fun new (s, x) =
         if s = 0 then empty ()
         else T {array = ref (Array.new (1, SOME x)),
                length = ref s}
      val array = new

      fun tabulate (s, f) =
         if s = 0 then empty ()
         else T {array = ref (Array.tabulate (s, fn i => SOME (f i))),
                length = ref s}

      fun subSafe (a, i) =
         case Array.sub (getArray a, i) of
            SOME x => x
          | NONE => Error.bug "ResizableArray.subSafe"

      fun sub (a, i) =
         if i < length a then subSafe (a, i)
         else Error.bug "ResizableArray.sub"

      fun updateSafe (a, i, x) =
         Array.update (getArray a, i, SOME x)

      fun update (a, i, x) =
         if i < length a then updateSafe (a, i, x)
         else Error.bug "ResizableArray.update"

      fun fromList l =
         let val a = Array.fromList (List.map (l, SOME))
         in T {array = ref a,
              length = ref (Array.length a)}
         end
   end

structure A'' =
   Array (open A'
          val unsafeSub = sub
          val unsafeUpdate = update
          val unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b t * 'a =
             fn (n, ac, f) =>
             let
                val (arr, z) =
                   Array.unfoldi (n, ac, fn (i, a) =>
                                  let
                                     val (b, a') = f (i, a)
                                  in (SOME b, a')
                                  end)
             in
                (T {array = ref arr,
                    length = ref n},
                 z)
             end)

open A' A''

fun subOption (a, i) =
   if i < length a
      then Array.sub (getArray a, i)
   else NONE

fun grow (a as T {array, ...}) =
   array := Array.tabulate (maxLength a * 2,
                       fn i => subOption (a, i))

fun shrink (a as T {array, ...}) =
   array := Array.tabulate (maxLength a div 2,
                       fn i => subOption (a, i))

fun addToEnd (a, x) =
   (if length a = maxLength a then grow a else ()
    ; updateSafe (a, length a, x)
    ; incLength a)

fun deleteLast a =
   if length a = 0
      then Error.bug "ResizableArray.deleteLast"
   else let val x = subSafe (a, maxIndex a)
        in (if length a = minLength a then shrink a else ()
            ; decLength a
            ; x)
        end

end

structure ResizableArray = ResizableArray ()
