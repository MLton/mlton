(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure Vector: VECTOR_EXTRA =
   struct
      structure V = Sequence (type 'a sequence = 'a vector
			      type 'a elt = 'a
			      val fromArray = Primitive.Vector.fromArray
			      open Primitive.Vector
			      val shareZero = true)
      open V

      type 'a vector = 'a vector

      val fromArray = Primitive.Vector.fromArray
      val unsafeSub = Primitive.Vector.sub
   end

structure VectorGlobal: VECTOR_GLOBAL = Vector
open VectorGlobal
val vector = Vector.fromList
