(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Array
   (Array:
    sig
       type 'a array
       type 'a elem
       type 'a vector
       val maxLen: int 
       val array: int * 'a elem -> 'a array 
       val fromList: 'a elem list -> 'a array 
       val tabulate: int * (int -> 'a elem) -> 'a array 
       val length: 'a array -> int 
       val sub: 'a array * int -> 'a elem
       val update: 'a array * int * 'a elem -> unit 
       val copy: {src: 'a array, dst: 'a array, di: int} -> unit 
       val copyVec: {src: 'a vector, dst: 'a array, di: int} -> unit 
       val appi: (int * 'a elem -> unit) -> 'a array -> unit 
       val app: ('a elem -> unit) -> 'a array -> unit 
       val foldli: (int * 'a elem * 'b -> 'b) -> 'b -> 'a array -> 'b
       val foldri: (int * 'a elem * 'b -> 'b) -> 'b -> 'a array -> 'b
       val foldl: ('a elem * 'b -> 'b) -> 'b -> 'a array -> 'b 
       val foldr: ('a elem * 'b -> 'b) -> 'b -> 'a array -> 'b 
       val modifyi: (int * 'a elem -> 'a elem) -> 'a array -> unit 
       val modify: ('a elem -> 'a elem) -> 'a array -> unit 
    end) =
   struct
      open Array OpenInt32

      val maxLen = fromInt maxLen
      fun array (n, x) = Array.array (toInt n, x)
      fun tabulate (n, f) = Array.tabulate (toInt n, f o fromInt)
      fun length a = fromInt (Array.length a)
      fun update (a, i, x) = Array.update (a, toInt i, x)
      fun sub (a, i: Int.int) = Array.sub (a, toInt i)
      fun convertSlice (a, i, io) = (a, toInt i, toIntOpt io)
      local
         fun doit (f, {src, dst, di}) =
            f {di = toInt di, dst = dst, src = src}
      in
         fun copy (f, a) = doit (Array.copy, a)
         fun copyVec (f, a) = doit (Array.copyVec, a)
      end
      fun appi f a = Array.appi (fn (i, x) => f (fromInt i, x)) a
      local
         fun make fold f b a =
            fold (fn (i, a, b) => f (fromInt i, a, b)) b a
      in
         fun foldli z = make Array.foldli z
         fun foldri z = make Array.foldri z
      end
      fun modifyi f a = Array.modifyi (fn (i, x) => f (fromInt i, x)) a
   end

structure Array =
   let 
      structure A = Array (open Array
                           type 'a elem = 'a)
   in struct open Array A end
   end

functor MonoArray (A: MONO_ARRAY) =
   let
      structure A' = Array (open A
                           type 'a array = array
                           type 'a vector = vector
                           type 'a elem = elem
                           (* The following rebindings are because of an
                            * SML/NJ bug.
                            *)
                           val app = app
                           val appi = appi
                           val array = array
                           val copy = copy
                           val copyVec = copyVec
                           val fromList = fromList
                           val length = length
                           val modify = modify
                           val modifyi = modifyi
                           val sub = sub
                           val tabulate = tabulate
                           val update = update)
   in struct
         open A A'
         local open A
         in type array = array
            type vector = vector
            type elem = elem
         end
      end
   end

structure CharArray = MonoArray (CharArray)
structure RealArray = MonoArray (RealArray)
structure Real64Array = RealArray
structure Word8Array = MonoArray (Word8Array)

functor MonoArraySlice (S: MONO_ARRAY_SLICE) =
   let
      open OpenInt32
   in
      struct
         type array = S.array
         type elem = S.elem
         type slice = S.slice
         type vector = S.vector
         type vector_slice = S.vector_slice

         val all = S.all

         val app = S.app

         fun appi f = S.appi (fn (i, e) => f (fromInt i, e))

         fun base s =
            let
               val (a, i, j) = S.base s
            in
               (a, fromInt i, fromInt j)
            end

         val collate = S.collate

         fun copy {di, dst, src} = S.copy {di = toInt di, dst = dst, src = src}

         fun copyVec {di, dst, src} =
            S.copyVec {di = toInt di, dst = dst, src = src}

         val exists = S.exists

         val find = S.find

         fun findi f s =
            case S.findi (fn (i, e) => f (fromInt i, e)) s of
               NONE => NONE
             | SOME (i, e) => SOME (fromInt i, e)

         val foldl = S.foldl

         fun foldli f = S.foldli (fn (i, e, b) => f (fromInt i, e, b))

         val foldr = S.foldr

         fun foldri f = S.foldri (fn (i, e, b) => f (fromInt i, e, b))

         val full = S.full

         val getItem = S.getItem

         val isEmpty = S.isEmpty

         val length = fromInt o S.length

         val modify = S.modify

         fun modifyi f = S.modifyi (fn (i, e) => f (fromInt i, e))

         fun slice (a, i, j) = S.slice (a, toInt i, toIntOpt j)

         fun sub (s, i) = S.sub (s, toInt i)

         fun subslice (s, i, j) = S.subslice (s, toInt i, toIntOpt j)

         fun update (s, i, e) = S.update (s, toInt i, e)

         val vector = S.vector
      end
   end

structure Word8ArraySlice = MonoArraySlice (Word8ArraySlice)
