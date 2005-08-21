(* Obtained at http://www.arrakis.es/~worm/ *)

signature MONO_VECTOR =
  sig
    type vector
    type elem
    val maxLen : int
    val fromList : elem list -> vector
    val tabulate : (int * (int -> elem)) -> vector
    val length : vector -> int
    val sub : (vector * int) -> elem
    val extract : (vector * int * int option) -> vector
    val concat : vector list -> vector
    val mapi : ((int * elem) -> elem) -> (vector * int * int option) -> vector
    val map : (elem -> elem) -> vector -> vector
    val appi : ((int * elem) -> unit) -> (vector * int * int option) -> unit
    val app : (elem -> unit) -> vector -> unit
    val foldli : ((int * elem * 'a) -> 'a) -> 'a -> (vector * int * int option) -> 'a
    val foldri : ((int * elem * 'a) -> 'a) -> 'a -> (vector * int * int option) -> 'a
    val foldl : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a
    val foldr : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a 
  end

(*
 Copyright (c) Juan Jose Garcia Ripoll.
 All rights reserved.

 Refer to the COPYRIGHT file for license conditions
*)

(* COPYRIGHT
 
Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the following
conditions are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following
   disclaimer in the documentation and/or other materials provided
   with the distribution.

3. All advertising materials mentioning features or use of this
   software must display the following acknowledgement:
        This product includes software developed by Juan Jose
        Garcia Ripoll.

4. The name of Juan Jose Garcia Ripoll may not be used to endorse
   or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY JUAN JOSE GARCIA RIPOLL ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL HE BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
OF SUCH DAMAGE.
*)

structure EvalTimer =
    struct
        local
            val TIME = ref (Time.now())
        in
            fun timerOn () =
                (TIME := Time.now(); ())
            fun timerRead () =
                Time.toMilliseconds(Time.-(Time.now(),!TIME))
            fun timerOff () =
                let val delta = timerRead()
                in
                    print "Elapsed: ";
                    print (LargeInt.toString delta);
                    print " ms\n"
                end
            fun time f = (timerOn(); f(); timerOff())
        end
    end
structure Loop =
    struct
        fun all (a, b, f) =
            if a > b then
                true
            else if f a then
                all (a+1, b, f)
            else
                false

        fun any (a, b, f) =
            if a > b then
                false
            else if f a then
                true
            else
                any (a+1, b, f)

        fun app (a, b, f) =
            if a < b then
                (f a; app (a+1, b, f))
            else
                ()

        fun app' (a, b, d, f) =
            if a < b then
                (f a; app' (a+d, b, d, f))
            else
                ()

        fun appi' (a, b, d, f) =
            if a < b then
                (f a; appi' (a+d, b, d, f))
            else
                ()
    end
(*
  INDEX         -Signature-

  Indices are a enumerable finite set of data with an order and a map
  to a continous nonnegative interval of integers.  In the sample
  implementation, Index, each index is a list of integers,
        [i1,...,in]
  and each set of indices is defined by a shape, which has the same
  shape of an index but with each integer incremented by one
        shape = [k1,...,kn]
        0 <= i1 < k1

  type storage = RowMajor | ColumnMajor
  order : storage
        Identifies:
                1) the underlying algorithms for this structure
                2) the most significant index
                3) the index that varies more slowly
                4) the total order
        RowMajor means that first index is most significant and varies
        more slowly, while ColumnMajor means that last index is the most
        significant and varies more slowly. For instance
                RowMajor => [0,0]<[0,1]<[1,0]<[1,1] (C, C++, Pascal)
                ColumnMajor => [0,0]>[1,0]>[0,1]>[1,1] (Fortran)
  last shape
  first shape
        Returns the last/first index that belongs to the sed defined by
        'shape'.
  inBounds shape index
        Checkes whether 'index' belongs to the set defined by 'shape'.
  toInt shape index
        As we said, indices can be sorted and mapped to a finite set of
        integers. 'toInt' obtaines the integer number that corresponds to
        a certain index.
  indexer shape
        It is equivalent to the partial evaluation 'toInt shape' but
        optimized for 'shape'.

  next shape index
  prev shape index
  next' shape index
  prev' shape index
        Obtain the following or previous index to the one we supply.
        next and prev return an object of type 'index option' so that
        if there is no such following/previous, the output is NONE.
        On the other hand, next'/prev' raise an exception when the
        output is not well defined and their output is always of type
        index. next/prev/next'/prev' raise an exception if 'index'
        does not belong to the set of 'shape'.

  all shape f
  any shape f
  app shape f
        Iterates 'f' over every index of the set defined by 'shape'.
        'all' stops when 'f' first returns false, 'any' stops when
        'f' first returns true and 'app' does not stop and discards the
        output of 'f'.

  compare(a,b)
        Returns LESS/GREATER/EQUAL according to the total order which
        is defined in the set of all indices.
  <,>,eq,<=,>=,<>
        Reduced comparisons which are defined in terms of 'compare'.

  validShape t
  validIndex t
        Checks whether 't' conforms a valid shape or index.

  iteri shape f
*)

signature INDEX =
    sig
        type t
        type indexer = t -> int
        datatype storage = RowMajor | ColumnMajor

        exception Index
        exception Shape

        val order : storage
        val toInt : t -> t -> int
        val length : t -> int
        val first : t -> t
        val last : t -> t
        val next : t -> t -> t option
        val prev : t -> t -> t option
        val next' : t -> t -> t
        val prev' : t -> t -> t
        val indexer : t -> (t -> int)

        val inBounds : t -> t -> bool
        val compare : t * t -> order
        val < : t * t -> bool
        val > : t * t -> bool
        val eq : t * t -> bool
        val <= : t * t -> bool
        val >= : t * t -> bool
        val <> : t * t -> bool
        val - : t * t -> t

        val validShape : t -> bool
        val validIndex : t -> bool

        val all : t -> (t -> bool) -> bool
        val any : t -> (t -> bool) -> bool
        val app : t -> (t -> unit) -> unit
    end
structure Index : INDEX =
    struct
        type t = int list
        type indexer = t -> int
        datatype storage = RowMajor | ColumnMajor

        exception Index
        exception Shape

        val order = ColumnMajor

        fun validShape shape = List.all (fn x => x > 0) shape

        fun validIndex index = List.all (fn x => x >= 0) index

        fun toInt shape index =
            let fun loop ([], [], accum, _) = accum
                  | loop ([], _, _, _) = raise Index
                  | loop (_, [], _, _) = raise Index
                  | loop (i::ri, l::rl, accum, fac) =
                if (i >= 0) andalso (i < l) then
                    loop (ri, rl, i*fac + accum, fac*l)
                else
                    raise Index
            in loop (index, shape, 0, 1)
            end

        (* ----- CACHED LINEAR INDEXER -----

           An indexer is a function that takes a list of
           indices, validates it and produces a nonnegative
           integer number. In short, the indexer is the
           mapper from indices to element positions in
           arrays.

           'indexer' builds such a mapper by optimizing
           the most common cases, which are 1d and 2d
           tensors.
         *)
    local
        fun doindexer [] _ = raise Shape
          | doindexer [a] [dx] =
            let fun f [x] = if (x > 0) andalso (x < a)
                            then x
                            else raise Index
                  | f _ = raise Index
            in f end
          | doindexer [a,b] [dx, dy] =
            let fun f [x,y] = if ((x > 0) andalso (x < a) andalso
                                  (y > 0) andalso (y < b))
                              then x + dy * y
                              else raise Index
                  | f _ = raise Index
            in f end
          | doindexer [a,b,c] [dx,dy,dz] =
            let fun f [x,y,z] = if ((x > 0) andalso (x < a) andalso
                                    (y > 0) andalso (y < b) andalso
                                    (z > 0) andalso (z < c))
                                then x + dy * y + dz * z
                                else raise Index
                  | f _ = raise Index
            in f end
          | doindexer shape memo =
            let fun f [] [] accum [] = accum
                  | f _  _  _ [] = raise Index
                  | f (fac::rf) (ndx::ri) accum (dim::rd) =
                    if (ndx >= 0) andalso (ndx < dim) then
                        f rf ri (accum + ndx * fac) rd
                    else
                        raise Index
            in f shape memo 0
            end
    in
        fun indexer shape =
            let fun memoize accum [] = []
                  | memoize accum (dim::rd) =
                    accum :: (memoize (dim * accum) rd)
            in
                if validShape shape
                then doindexer shape (memoize 1 shape)
                else raise Shape
            end
    end

        fun length shape =
            let fun prod (a,b) =
                if b < 0 then raise Shape else a * b
            in foldl prod 1 shape
            end

        fun first shape = map (fn x => 0) shape

        fun last [] = []
          | last (size :: rest) =
            if size < 1
            then raise Shape
            else size - 1 :: last rest

        fun next' [] [] = raise Subscript
          | next' _ [] = raise Index
          | next' [] _ = raise Index
          | next' (dimension::restd) (index::resti) =
            if (index + 1) < dimension
            then (index + 1) :: resti
            else 0 :: (next' restd resti)

        fun prev' [] [] = raise Subscript
          | prev' _ [] = raise Index
          | prev' [] _ = raise Index
          | prev' (dimension::restd) (index::resti) =
            if (index > 0)
            then index - 1 :: resti
            else dimension - 1 :: prev' restd resti

        fun next shape index = (SOME (next' shape index)) handle
            Subscript => NONE

        fun prev shape index = (SOME (prev' shape index)) handle
            Subscript => NONE

        fun inBounds shape index =
            ListPair.all (fn (x,y) => (x >= 0) andalso (x < y))
            (index, shape)

        fun compare ([],[]) = EQUAL
          | compare (_, []) = raise Index
          | compare ([],_) = raise Index
          | compare (a::ra, b::rb) =
            case Int.compare (a,b) of
                EQUAL => compare (ra,rb)
              | LESS => LESS
              | GREATER => GREATER

    local
        fun iterator a inner =
            let fun loop accum f =
                let fun innerloop i =
                    if i < a
                    then if inner (i::accum) f
                         then innerloop (i+1)
                         else false
                    else true
                in innerloop 0
                end
            in loop
            end
        fun build_iterator [a] =
            let fun loop accum f =
                let fun innerloop i =
                    if i < a
                    then if f (i::accum)
                         then innerloop (i+1)
                         else false
                    else true
                in innerloop 0
                end
            in loop
            end
          | build_iterator (a::rest) = iterator a (build_iterator rest)
    in
        fun all shape = build_iterator shape []
    end

    local
        fun iterator a inner =
            let fun loop accum f =
                let fun innerloop i =
                    if i < a
                    then if inner (i::accum) f
                         then true
                         else innerloop (i+1)
                    else false
                in innerloop 0
                end
            in loop
            end
        fun build_iterator [a] =
            let fun loop accum f =
                let fun innerloop i =
                    if i < a
                    then if f (i::accum)
                         then true
                         else innerloop (i+1)
                    else false
                in innerloop 0
                end
            in loop
            end
          | build_iterator (a::rest) = iterator a (build_iterator rest)
    in
        fun any shape = build_iterator shape []
    end

    local
        fun iterator a inner =
            let fun loop accum f =
                let fun innerloop i =
                    if i < a
                    then (inner (i::accum) f;
                          innerloop (i+1))
                    else ()
                in innerloop 0
                end
            in loop
            end
        fun build_iterator [a] =
            let fun loop accum f =
                let fun innerloop i =
                    if i < a
                    then (f (i::accum); innerloop (i+1))
                    else ()
                in innerloop 0
                end
            in loop
            end
          | build_iterator (a::rest) = iterator a (build_iterator rest)
    in
        fun app shape = build_iterator shape []
    end

        fun a < b = compare(a,b) = LESS
        fun a > b = compare(a,b) = GREATER
        fun eq (a, b) = compare(a,b) = EQUAL
        fun a <> b = not (a = b)
        fun a <= b = not (a > b)
        fun a >= b = not (a < b)
        fun a - b = ListPair.map Int.- (a,b)

    end
(*
 Copyright (c) Juan Jose Garcia Ripoll.
 All rights reserved.
 
 Refer to the COPYRIGHT file for license conditions
*)

(*
 TENSOR         - Signature -

 Polymorphic tensors of any type. With 'tensor' we denote a (mutable)
 array of any rank, with as many indices as one wishes, and that may
 be traversed (map, fold, etc) according to any of those indices.

 type 'a tensor
        Polymorphic tensor whose elements are all of type 'a.
 val storage = RowMajor | ColumnMajor
        RowMajor = data is stored in consecutive cells, first index
        varying fastest (FORTRAN convention)
        ColumnMajor = data is stored in consecutive cells, last
        index varying fastest (C,C++,Pascal,CommonLisp convention)
 new ([i1,...,in],init)
        Build a new tensor with n indices, each of sizes i1...in,
        filled with 'init'.
 fromArray (shape,data)
 fromList (shape,data)
        Use 'data' to fill a tensor of that shape. An exception is
        raised if 'data' is too large or too small to properly
        fill the vector. Later use of a 'data' array is disregarded
        -- one must think that the tensor now owns the array.
 length tensor
 rank tensor
 shape tensor
        Return the number of elements, the number of indices and
        the shape (size of each index) of the tensor.
 toArray tensor
        Return the data of the tensor in the form of an array.
        Mutation of this array may lead to unexpected behavior.

 sub (tensor,[i1,...,in])
 update (tensor,[i1,...,in],new_value)
        Access the element that is indexed by the numbers [i1,..,in]

 app f a
 appi f a
        The same as 'map' and 'mapi' but the function 'f' outputs
        nothing and no new array is produced, i.e. one only seeks
        the side effect that 'f' may produce.
 map2 operation a b
        Apply function 'f' to pairs of elements of 'a' and 'b'
        and build a new tensor with the output. Both operands
        must have the same shape or an exception is raised.
        The procedure is sequential, as specified by 'storage'.
 foldl operation a n
        Fold-left the elements of tensor 'a' along the n-th
        index.
 all test a
 any test a
        Folded boolean tests on the elements of the tensor.
*)

signature TENSOR =
    sig
        structure Array : ARRAY
        structure Index : INDEX
        type index = Index.t
        type 'a tensor

        val new : index * 'a -> 'a tensor
        val tabulate : index * (index -> 'a) -> 'a tensor
        val length : 'a tensor -> int
        val rank : 'a tensor -> int
        val shape : 'a tensor -> (index)
        val reshape : index -> 'a tensor -> 'a tensor
        val fromList : index * 'a list -> 'a tensor
        val fromArray : index * 'a array -> 'a tensor
        val toArray : 'a tensor -> 'a array

        val sub : 'a tensor * index -> 'a
        val update : 'a tensor * index * 'a -> unit
        val map : ('a -> 'b) -> 'a tensor -> 'b tensor
        val map2 : ('a * 'b -> 'c) -> 'a tensor -> 'b tensor -> 'c tensor
        val app : ('a -> unit) -> 'a tensor -> unit
        val appi : (int * 'a -> unit) -> 'a tensor -> unit
        val foldl : ('c * 'a -> 'c) -> 'c -> 'a tensor -> int -> 'c tensor
        val all : ('a -> bool) -> 'a tensor -> bool
        val any : ('a -> bool) -> 'a tensor -> bool
    end

(*
 Copyright (c) Juan Jose Garcia Ripoll.
 All rights reserved.
 
 Refer to the COPYRIGHT file for license conditions
*)

structure Tensor : TENSOR =
    struct
        structure Array = Array
        structure Index = Index
            
        type index = Index.t
        type 'a tensor = {shape : index, indexer : Index.indexer, data : 'a array}

        exception Shape
        exception Match
        exception Index

    local
    (*----- LOCALS -----*)

        fun make' (shape, data) =
            {shape = shape, indexer = Index.indexer shape, data = data}

        fun toInt {shape, indexer, data} index = indexer index

        fun array_map f a =
            let fun apply index = f(Array.sub(a,index)) in
                Array.tabulate(Array.length a, apply)
            end

        fun splitList (l as (a::rest), place) =
            let fun loop (left,here,right) 0 =  (List.rev left,here,right)
                  | loop (_,_,[]) place = raise Index
                  | loop (left,here,a::right) place = 
                loop (here::left,a,right) (place-1)
            in
                if place <= 0 then
                    loop ([],a,rest) (List.length rest - place)
                else
                    loop ([],a,rest) (place - 1)
            end

    in
    (*----- STRUCTURAL OPERATIONS & QUERIES ------*)

        fun new (shape, init) =
            if not (Index.validShape shape) then
                raise Shape
            else
                let val length = Index.length shape in
                    {shape = shape,
                     indexer = Index.indexer shape,
                     data = Array.array(length,init)}
                end

        fun toArray {shape, indexer, data} = data

        fun length {shape, indexer, data} =  Array.length data

        fun shape {shape, indexer, data} = shape

        fun rank t = List.length (shape t)

        fun reshape new_shape tensor =
            if Index.validShape new_shape then
                case (Index.length new_shape) = length tensor of
                    true => make'(new_shape, toArray tensor)
                  | false => raise Match
            else
                raise Shape

        fun fromArray (s, a) =
            case Index.validShape s andalso 
                 ((Index.length s) = (Array.length a)) of
                 true => make'(s, a)
               | false => raise Shape

        fun fromList (s, a) = fromArray (s, Array.fromList a)

        fun tabulate (shape,f) =
            if Index.validShape shape then
                let val last = Index.last shape
                    val length = Index.length shape
                    val c = Array.array(length, f last)
                    fun dotable (c, indices, i) =
                        (Array.update(c, i, f indices);
                         case i of
                             0 => c
                           | i => dotable(c, Index.prev' shape indices, i-1))
                in
                    make'(shape,dotable(c, Index.prev' shape last, length-1))
                end
            else
                raise Shape

        (*----- ELEMENTWISE OPERATIONS -----*)

        fun sub (t, index) = Array.sub(#data t, toInt t index)

        fun update (t, index, value) =
            Array.update(toArray t, toInt t index, value)

        fun map f {shape, indexer, data} =
            {shape = shape, indexer = indexer, data = array_map f data}

        fun map2 f t1 t2=
            let val {shape, indexer, data} = t1
                val {shape=shape2, indexer=indexer2, data=data2} = t2
                fun apply i = f (Array.sub(data,i), Array.sub(data2,i))
                val len = Array.length data
            in
                if Index.eq(shape, shape2) then
                    {shape = shape,
                     indexer = indexer,
                     data = Array.tabulate(len, apply)}
                else
                    raise Match
        end

        fun appi f tensor = Array.appi f (toArray tensor)

        fun app f tensor = Array.app f (toArray tensor)

        fun all f tensor =
            let val a = toArray tensor
            in Loop.all(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end

        fun any f tensor =
            let val a = toArray tensor
            in Loop.any(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end

        fun foldl f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Array.update(c, ic, f(Array.sub(c,ic), Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end

    end
    end (* Tensor *)

(*
 Copyright (c) Juan Jose Garcia Ripoll.
 All rights reserved.
 
 Refer to the COPYRIGHT file for license conditions
*)

(*
 MONO_TENSOR            - signature -

 Monomorphic tensor of arbitrary data (not only numbers). Operations
 should be provided to run the data in several ways, according to one
 index.

 type tensor
        The type of the tensor itself
 type elem
        The type of every element
 val storage = RowMajor | ColumnMajor
        RowMajor = data is stored in consecutive cells, first index
        varying fastest (FORTRAN convention)
        ColumnMajor = data is stored in consecutive cells, last
        index varying fastest (C,C++,Pascal,CommonLisp convention)
 new ([i1,...,in],init)
        Build a new tensor with n indices, each of sizes i1...in,
        filled with 'init'.
 fromArray (shape,data)
 fromList (shape,data)
        Use 'data' to fill a tensor of that shape. An exception is
        raised if 'data' is too large or too small to properly
        fill the vector. Later use of a 'data' array is disregarded
        -- one must think that the tensor now owns the array.
 length tensor
 rank tensor
 shape tensor
        Return the number of elements, the number of indices and
        the shape (size of each index) of the tensor.
 toArray tensor
        Return the data of the tensor in the form of an array.
        Mutation of this array may lead to unexpected behavior.
        The data in the array is stored according to `storage'.

 sub (tensor,[i1,...,in])
 update (tensor,[i1,...,in],new_value)
        Access the element that is indexed by the numbers [i1,..,in]

 map f a
 mapi f a
        Produce a new array by mapping the function sequentially
        as specified by 'storage', to each element of tensor 'a'.
        In 'mapi' the function receives a (indices,value) tuple,
        while in 'map' it only receives the value.
 app f a
 appi f a
        The same as 'map' and 'mapi' but the function 'f' outputs
        nothing and no new array is produced, i.e. one only seeks
        the side effect that 'f' may produce.
 map2 operation a b
        Apply function 'f' to pairs of elements of 'a' and 'b'
        and build a new tensor with the output. Both operands
        must have the same shape or an exception is raised.
        The procedure is sequential, as specified by 'storage'.
 foldl operation a n
        Fold-left the elements of tensor 'a' along the n-th
        index.
 all test a
 any test a
        Folded boolean tests on the elements of the tensor.

 map', map2', foldl'
        Polymorphic versions of map, map2, foldl.
*)

signature MONO_TENSOR =
    sig
        structure Array : MONO_ARRAY
        structure Index : INDEX
        type index = Index.t
        type elem
        type tensor
        type t = tensor

        val new : index * elem -> tensor
        val tabulate : index * (index -> elem) -> tensor
        val length : tensor -> int
        val rank : tensor -> int
        val shape : tensor -> (index)
        val reshape : index -> tensor -> tensor
        val fromList : index * elem list -> tensor
        val fromArray : index * Array.array -> tensor
        val toArray : tensor -> Array.array

        val sub : tensor * index -> elem
        val update : tensor * index * elem -> unit
        val map : (elem -> elem) -> tensor -> tensor
        val map2 : (elem * elem -> elem) -> tensor -> tensor -> tensor
        val app : (elem -> unit) -> tensor -> unit
        val appi : (int * elem -> unit) -> tensor -> unit
        val foldl : (elem * 'a -> 'a) -> 'a -> tensor -> tensor
        val foldln : (elem * elem -> elem) -> elem -> tensor -> int -> tensor
        val all : (elem -> bool) -> tensor -> bool
        val any : (elem -> bool) -> tensor -> bool

        val map' : (elem -> 'a) -> tensor -> 'a Tensor.tensor
        val map2' : (elem * elem -> 'a) -> tensor -> tensor -> 'a Tensor.tensor
        val foldl' : ('a * elem -> 'a) -> 'a -> tensor -> int -> 'a Tensor.tensor
    end

(*
 NUMBER         - Signature -

 Guarantees a structure with a minimal number of mathematical operations
 so as to build an algebraic structure named Tensor.
 *)

signature NUMBER =
    sig
        type t
        val zero : t
        val one  : t
        val ~ : t -> t
        val + : t * t -> t
        val - : t * t -> t
        val * : t * t -> t
        val / : t * t -> t
        val toString : t -> string
    end

signature NUMBER =
    sig
        type t
        val zero : t
        val one : t

        val + : t * t -> t
        val - : t * t -> t
        val * : t * t -> t
        val *+ : t * t * t -> t
        val *- : t * t * t -> t
        val ** : t * int -> t

        val ~ : t -> t
        val abs : t -> t
        val signum : t -> t

        val == : t * t -> bool
        val != : t * t -> bool

        val toString : t -> string
        val fromInt : int -> t
        val scan : (char,'a) StringCvt.reader -> (t,'a) StringCvt.reader
    end

signature INTEGRAL_NUMBER =
    sig
        include NUMBER

        val quot : t * t -> t
        val rem  : t * t -> t
        val mod  : t * t -> t
        val div  : t * t -> t

        val compare : t * t -> order
        val < : t * t -> bool
        val > : t * t -> bool
        val <= : t * t -> bool
        val >= : t * t -> bool

        val max : t * t -> t
        val min : t * t -> t
    end

signature FRACTIONAL_NUMBER =
    sig
        include NUMBER

        val pi : t
        val e : t

        val / : t * t -> t
        val recip : t -> t

        val ln : t -> t
        val pow : t * t -> t
        val exp : t -> t
        val sqrt : t -> t

        val cos : t -> t
        val sin : t -> t
        val tan : t -> t
        val sinh : t -> t
        val cosh : t -> t
        val tanh : t -> t

        val acos : t -> t
        val asin : t -> t
        val atan : t -> t
        val asinh : t -> t
        val acosh : t -> t
        val atanh : t -> t
        val atan2 : t * t -> t
    end

signature REAL_NUMBER =
    sig
        include FRACTIONAL_NUMBER

        val compare : t * t -> order
        val < : t * t -> bool
        val > : t * t -> bool
        val <= : t * t -> bool
        val >= : t * t -> bool

        val max : t * t -> t
        val min : t * t -> t
    end

signature COMPLEX_NUMBER =
    sig
        include FRACTIONAL_NUMBER

        structure Real : REAL_NUMBER
        type real = Real.t

        val make : real * real -> t
        val split : t -> real * real
        val realPart : t -> real
        val imagPart : t -> real
        val abs2 : t -> real
    end

structure INumber : INTEGRAL_NUMBER =
    struct
        open Int
        type t = Int.int
        val zero = 0
        val one = 1

        infix **
        fun i ** n =
            let fun loop 0 = 1
                  | loop 1 = i
                  | loop n =
                let val x = loop (Int.div(n, 2))
                    val m = Int.mod(n, 2)
                in
                    if m = 0 then
                        x * x
                    else
                        x * x * i
                end
            in if n < 0
               then raise Domain
               else loop n
            end

        fun signum i = case compare(i, 0) of
            GREATER => 1
          | EQUAL => 0
          | LESS => ~1

        infix ==
        infix !=
        fun a == b = a = b
        fun a != b = (a <> b)
        fun *+(b,c,a) = b * c + a
        fun *-(b,c,a) = b * c - b

        fun scan getc = Int.scan StringCvt.DEC getc
    end

structure RNumber : REAL_NUMBER =
    struct
        open Real
        open Real.Math
        type t = Real.real
        val zero = 0.0
        val one = 1.0

        fun signum x = case compare(x,0.0) of
            LESS => ~1.0
          | GREATER => 1.0
          | EQUAL => 0.0

        fun recip x = 1.0 / x

        infix **
        fun i ** n =
            let fun loop 0 = one
                  | loop 1 = i
                  | loop n =
                let val x = loop (Int.div(n, 2))
                    val m = Int.mod(n, 2)
                in
                    if m = 0 then
                        x * x
                    else
                        x * x * i
                end
            in if Int.<(n, 0)
               then raise Domain
               else loop n
            end

        fun max (a, b) = if a < b then b else a
        fun min (a, b) = if a < b then a else b

        fun asinh x = ln (x + sqrt(1.0 + x * x))
        fun acosh x = ln (x + (x + 1.0) * sqrt((x - 1.0)/(x + 1.0)))
        fun atanh x = ln ((1.0 + x) / sqrt(1.0 - x * x))

    end
(*
 Complex(R)     - Functor -

 Provides support for complex numbers based on tuples. Should be
 highly efficient as most operations can be inlined.
 *)

structure CNumber : COMPLEX_NUMBER =
struct
        structure Real = RNumber

        type t = Real.t * Real.t
        type real = Real.t

        val zero = (0.0,0.0)
        val one = (1.0,0.0)
        val pi = (Real.pi, 0.0)
        val e = (Real.e, 0.0)

        fun make (r,i) = (r,i) : t
        fun split z = z
        fun realPart (r,_) = r
        fun imagPart (_,i) = i

        fun abs2 (r,i) = Real.+(Real.*(r,r),Real.*(i,i)) (* FIXME!!! *)
        fun arg (r,i) = Real.atan2(i,r)
        fun modulus z = Real.sqrt(abs2 z)
        fun abs z = (modulus z, 0.0)
        fun signum (z as (r,i)) =
            let val m = modulus z
            in (Real./(r,m), Real./(i,m))
            end

        fun ~ (r1,i1) = (Real.~ r1, Real.~ i1)
        fun (r1,i1) + (r2,i2) = (Real.+(r1,r2), Real.+(i1,i2))
        fun (r1,i1) - (r2,i2) = (Real.-(r1,r2), Real.-(i1,i1))
        fun (r1,i1) * (r2,i2) = (Real.-(Real.*(r1,r2),Real.*(i1,i2)),
                                 Real.+(Real.*(r1,i2),Real.*(r2,i1)))
        fun (r1,i1) / (r2,i2) =
            let val modulus = abs2(r2,i2)
                val (nr,ni) = (r1,i1) * (r2,i2)
            in
                (Real./(nr,modulus), Real./(ni,modulus))
            end
        fun *+((r1,i1),(r2,i2),(r0,i0)) =
            (Real.*+(Real.~ i1, i2, Real.*+(r1,r2,r0)),
             Real.*+(r2, i2, Real.*+(r1,i2,i0)))
        fun *-((r1,i1),(r2,i2),(r0,i0)) =
            (Real.*+(Real.~ i1, i2, Real.*-(r1,r2,r0)),
             Real.*+(r2, i2, Real.*-(r1,i2,i0)))

        infix **
        fun i ** n =
            let fun loop 0 = one
                  | loop 1 = i
                  | loop n =
                let val x = loop (Int.div(n, 2))
                    val m = Int.mod(n, 2)
                in
                    if m = 0 then
                        x * x
                    else
                        x * x * i
                end
            in if Int.<(n, 0)
                   then raise Domain
               else loop n
            end

        fun recip (r1, i1) = 
            let val modulus = abs2(r1, i1)
            in (Real./(r1, modulus), Real./(Real.~ i1, modulus))
            end
        fun ==(z, w) = Real.==(realPart z, realPart w) andalso Real.==(imagPart z, imagPart w)
        fun !=(z, w) = Real.!=(realPart z, realPart w) andalso Real.!=(imagPart z, imagPart w)
        fun fromInt i = (Real.fromInt i, 0.0)
        fun toString (r,i) =
            String.concat ["(",Real.toString r,",",Real.toString i,")"]

        fun exp (x, y) =
            let val expx = Real.exp x
            in (Real.*(x, (Real.cos y)), Real.*(x, (Real.sin y)))
            end

    local
        val half = Real.recip (Real.fromInt 2)
    in
        fun sqrt (z as (x,y)) =
            if Real.==(x, 0.0) andalso Real.==(y, 0.0) then
                zero
            else
                let val m = Real.+(modulus z, Real.abs x)
                    val u' = Real.sqrt (Real.*(m, half))
                    val v' = Real./(Real.abs y , Real.+(u',u'))
                    val (u,v) = if Real.<(x, 0.0) then (v',u') else (u',v')
                in (u, if Real.<(y, 0.0) then Real.~ v else v)
                end
    end
        fun ln z = (Real.ln (modulus z), arg z)

        fun pow (z, n) =
            let val l = ln z
            in exp (l * n)
            end

        fun sin (x, y) = (Real.*(Real.sin x, Real.cosh y),
                          Real.*(Real.cos x, Real.sinh y))
        fun cos (x, y) = (Real.*(Real.cos x, Real.cosh y),
                          Real.~ (Real.*(Real.sin x, Real.sinh y)))
        fun tan (x, y) =
            let val (sx, cx) = (Real.sin x, Real.cos x)
                val (shy, chy) = (Real.sinh y, Real.cosh y)
                val a = (Real.*(sx, chy), Real.*(cx, shy))
                val b = (Real.*(cx, chy), Real.*(Real.~ sx, shy))
            in a / b
            end

        fun sinh (x, y) = (Real.*(Real.cos y, Real.sinh x),
                           Real.*(Real.sin y, Real.cosh x))
        fun cosh (x, y) = (Real.*(Real.cos y, Real.cosh x),
                           Real.*(Real.sin y, Real.sinh x))
        fun tanh (x, y) =
            let val (sy, cy) = (Real.sin y, Real.cos y)
                val (shx, chx) = (Real.sinh x, Real.cosh x)
                val a = (Real.*(cy, shx), Real.*(sy, chx))
                val b = (Real.*(cy, chx), Real.*(sy, shx))
            in a / b
            end

        fun asin (z as (x,y)) =
            let val w = sqrt (one - z * z)
                val (x',y') = ln ((Real.~ y, x) + w)
            in (y', Real.~ x')
            end

        fun acos (z as (x,y)) = 
            let val (x', y') = sqrt (one + z * z)
                val (x'', y'') = ln (z + (Real.~ y', x'))
            in (y'', Real.~ x'')
            end

        fun atan (z as (x,y)) =
            let val w = sqrt (one + z*z)
                val (x',y') = ln ((Real.-(1.0, y), x) / w)
            in (y', Real.~ x')
            end

        fun atan2 (y, x) = atan(y / x)

        fun asinh x = ln (x + sqrt(one + x * x))
        fun acosh x = ln (x + (x + one) * sqrt((x - one)/(x + one)))
        fun atanh x = ln ((one + x) / sqrt(one - x * x))

        fun scan getc =
            let val scanner = Real.scan getc
            in fn stream => 
                  case scanner stream of
                      NONE => NONE
                    | SOME (a, rest) =>
                      case scanner rest of
                          NONE => NONE
                        | SOME (b, rest) => SOME (make(a,b), rest)
            end

end (* ComplexNumber *)

(*
 Copyright (c) Juan Jose Garcia Ripoll.
 All rights reserved.
 
 Refer to the COPYRIGHT file for license conditions
*)

structure INumberArray =
    struct
        open Array
        type array = INumber.t array
        type vector = INumber.t vector
        type elem  = INumber.t
        structure Vector =
            struct
                open Vector
                type vector = INumber.t Vector.vector
                type elem = INumber.t
            end
        fun map f a = tabulate(length a, fn x => (f (sub(a,x))))
        fun mapi f a = tabulate(length a, fn x => (f (x,sub(a,x))))
        fun map2 f a b = tabulate(length a, fn x => (f(sub(a,x),sub(b,x))))
    end

structure RNumberArray =
    struct
        open Real64Array
        val sub = Unsafe.Real64Array.sub
        val update = Unsafe.Real64Array.update
        fun map f a = tabulate(length a, fn x => (f (sub(a,x))))
        fun mapi f a = tabulate(length a, fn x => (f (x,sub(a,x))))
        fun map2 f a b = tabulate(length a, fn x => (f(sub(a,x),sub(b,x))))
    end

(*--------------------- COMPLEX ARRAY -------------------------*)

structure BasicCNumberArray =
struct
        structure Complex : COMPLEX_NUMBER = CNumber
        structure Array : MONO_ARRAY = RNumberArray

        type elem = Complex.t
        type array = Array.array * Array.array

        val maxLen = Array.maxLen

        fun length (a,b) = Array.length a

        fun sub ((a,b),index) = Complex.make(Array.sub(a,index),Array.sub(b,index))

        fun update ((a,b),index,z) =
            let val (re,im) = Complex.split z in
                Array.update(a, index, re);
                Array.update(b, index, im)
            end

    local
        fun makeRange (a, start, NONE) = makeRange(a, start, SOME (length a - 1))
          | makeRange (a, start, SOME last) =
            let val len = length a
                val diff = last - start
            in
                if (start >= len) orelse (last >= len) then
                    raise Subscript
                else if diff < 0 then
                    (a, start, 0)
                else
                    (a, start, diff + 1)
            end

    in

        fun array (size,z:elem) =
            let val realsize = size * 2
                val r = Complex.realPart z
                val i = Complex.imagPart z in
                    (Array.array(size,r), Array.array(size,i))
            end

        fun zeroarray size =
            (Array.array(size,Complex.Real.zero),
             Array.array(size,Complex.Real.zero))

        fun tabulate (size,f) =
            let val a = array(size, Complex.zero)
                fun loop i =
                    case i = size of
                        true => a
                      | false => (update(a, i, f i); loop (i+1))
            in
                loop 0
            end

        fun fromList list =
            let val length = List.length list
                val a = zeroarray length
                fun loop (_, []) = a
                  | loop (i, z::rest) = (update(a, i, z);
                                         loop (i+1, rest))
            in
                loop(0,list)
            end

        fun extract range =
            let val (a, start, len) = makeRange range
                fun copy i = sub(a, i + start)
            in tabulate(len, copy)
            end

        fun concat array_list =
            let val total_length = foldl (op +) 0 (map length array_list)
                val a = array(total_length, Complex.zero)
                fun copy (_, []) = a
                  | copy (pos, v::rest) =
                    let fun loop i =
                        case i = 0 of
                            true => ()
                          | false => (update(a, i+pos, sub(v, i)); loop (i-1))
                    in (loop (length v - 1); copy(length v + pos, rest))
                    end
            in
                copy(0, array_list)
            end

        fun copy {src : array, si : int, len : int option, dst : array, di : int } =
            let val (a, ia, la) = makeRange (src, si, len)
                val (b, ib, lb) = makeRange (dst, di, len)
                fun copy i =
                    case i < 0 of
                        true => ()
                      | false => (update(b, i+ib, sub(a, i+ia)); copy (i-1))
            in copy (la - 1)
            end

        val copyVec = copy

        fun modifyi f range =
            let val (a, start, len) = makeRange range
                val last = start + len
                fun loop i =
                    case i >= last of
                        true => ()
                      | false => (update(a, i, f(i, sub(a,i))); loop (i+1))
            in loop start
            end

        fun modify f a =
            let val last = length a
                fun loop i =
                    case i >= last of
                        true => ()
                      | false => (update(a, i, f(sub(a,i))); loop (i+1))
            in loop 0
            end

        fun app f a =
            let val size = length a
                fun loop i =
                    case i = size of
                        true => ()
                      | false => (f(sub(a,i)); loop (i+1))
            in
                loop 0
            end

        fun appi f range =
            let val (a, start, len) = makeRange range
                val last = start + len
                fun loop i =
                    case i >= last of
                        true => ()
                      | false => (f(i, sub(a,i)); loop (i+1))
            in
                loop start
            end

        fun map f a =
            let val len = length a
                val c = zeroarray len
                fun loop ~1 = c
                  | loop i = (update(a, i, f(sub(a,i))); loop (i-1))
            in loop (len-1)
            end

        fun map2 f a b =
            let val len = length a
                val c = zeroarray len
                fun loop ~1 = c
                  | loop i = (update(c, i, f(sub(a,i),sub(b,i)));
                              loop (i-1))
            in loop (len-1)
            end

        fun mapi f range =
            let val (a, start, len) = makeRange range
                fun rule i = f (i+start, sub(a, i+start))
            in tabulate(len, rule)
            end

        fun foldli f init range =
            let val (a, start, len) = makeRange range
                val last = start + len - 1
                fun loop (i, accum) =
                    case i > last of
                        true => accum
                      | false => loop (i+1, f(i, sub(a,i), accum))
            in loop (start, init)
            end

        fun foldri f init range =
            let val (a, start, len) = makeRange range
                val last = start + len - 1
                fun loop (i, accum) =
                    case i < start of
                        true => accum
                      | false => loop (i-1, f(i, sub(a,i), accum))
            in loop (last, init)
            end

        fun foldl f init a = foldli (fn (_, a, x) => f(a,x)) init (a,0,NONE)
        fun foldr f init a = foldri (fn (_, x, a) => f(x,a)) init (a,0,NONE)
    end
end (* BasicCNumberArray *)


structure CNumberArray =
    struct
        structure Vector =
            struct
                open BasicCNumberArray
                type vector = array
            end : MONO_VECTOR
        type vector = Vector.vector
        open BasicCNumberArray
    end (* CNumberArray *)
structure INumber : INTEGRAL_NUMBER =
    struct
        open Int
        type t = Int.int
        val zero = 0
        val one = 1
        infix **
        fun i ** n =
            let fun loop 0 = 1
                  | loop 1 = i
                  | loop n =
                let val x = loop (Int.div(n, 2))
                    val m = Int.mod(n, 2)
                in
                    if m = 0 then
                        x * x
                    else
                        x * x * i
                end
            in if n < 0
               then raise Domain
               else loop n
            end
        fun signum i = case compare(i, 0) of
            GREATER => 1
          | EQUAL => 0
          | LESS => ~1
        infix ==
        infix !=
        fun a == b = a = b
        fun a != b = (a <> b)
        fun *+(b,c,a) = b * c + a
        fun *-(b,c,a) = b * c - b
        fun scan getc = Int.scan StringCvt.DEC getc
    end
structure RNumber : REAL_NUMBER =
    struct
        open Real
        open Real.Math
        type t = Real.real
        val zero = 0.0
        val one = 1.0
        fun signum x = case compare(x,0.0) of
            LESS => ~1.0
          | GREATER => 1.0
          | EQUAL => 0.0
        fun recip x = 1.0 / x
        infix **
        fun i ** n =
            let fun loop 0 = one
                  | loop 1 = i
                  | loop n =
                let val x = loop (Int.div(n, 2))
                    val m = Int.mod(n, 2)
                in
                    if m = 0 then
                        x * x
                    else
                        x * x * i
                end
            in if Int.<(n, 0)
               then raise Domain
               else loop n
            end
        fun max (a, b) = if a < b then b else a
        fun min (a, b) = if a < b then a else b
        fun asinh x = ln (x + sqrt(1.0 + x * x))
        fun acosh x = ln (x + (x + 1.0) * sqrt((x - 1.0)/(x + 1.0)))
        fun atanh x = ln ((1.0 + x) / sqrt(1.0 - x * x))
    end
(*
 Complex(R)     - Functor -
 Provides support for complex numbers based on tuples. Should be
 highly efficient as most operations can be inlined.
 *)
structure CNumber : COMPLEX_NUMBER =
struct
        structure Real = RNumber
        type t = Real.t * Real.t
        type real = Real.t
        val zero = (0.0,0.0)
        val one = (1.0,0.0)
        val pi = (Real.pi, 0.0)
        val e = (Real.e, 0.0)
        fun make (r,i) = (r,i) : t
        fun split z = z
        fun realPart (r,_) = r
        fun imagPart (_,i) = i
        fun abs2 (r,i) = Real.+(Real.*(r,r),Real.*(i,i)) (* FIXME!!! *)
        fun arg (r,i) = Real.atan2(i,r)
        fun modulus z = Real.sqrt(abs2 z)
        fun abs z = (modulus z, 0.0)
        fun signum (z as (r,i)) =
            let val m = modulus z
            in (Real./(r,m), Real./(i,m))
            end
        fun ~ (r1,i1) = (Real.~ r1, Real.~ i1)
        fun (r1,i1) + (r2,i2) = (Real.+(r1,r2), Real.+(i1,i2))
        fun (r1,i1) - (r2,i2) = (Real.-(r1,r2), Real.-(i1,i1))
        fun (r1,i1) * (r2,i2) = (Real.-(Real.*(r1,r2),Real.*(i1,i2)),
                                 Real.+(Real.*(r1,i2),Real.*(r2,i1)))
        fun (r1,i1) / (r2,i2) =
            let val modulus = abs2(r2,i2)
                val (nr,ni) = (r1,i1) * (r2,i2)
            in
                (Real./(nr,modulus), Real./(ni,modulus))
            end
        fun *+((r1,i1),(r2,i2),(r0,i0)) =
            (Real.*+(Real.~ i1, i2, Real.*+(r1,r2,r0)),
             Real.*+(r2, i2, Real.*+(r1,i2,i0)))
        fun *-((r1,i1),(r2,i2),(r0,i0)) =
            (Real.*+(Real.~ i1, i2, Real.*-(r1,r2,r0)),
             Real.*+(r2, i2, Real.*-(r1,i2,i0)))
        infix **
        fun i ** n =
            let fun loop 0 = one
                  | loop 1 = i
                  | loop n =
                let val x = loop (Int.div(n, 2))
                    val m = Int.mod(n, 2)
                in
                    if m = 0 then
                        x * x
                    else
                        x * x * i
                end
            in if Int.<(n, 0)
                   then raise Domain
               else loop n
            end
        fun recip (r1, i1) = 
            let val modulus = abs2(r1, i1)
            in (Real./(r1, modulus), Real./(Real.~ i1, modulus))
            end
        fun ==(z, w) = Real.==(realPart z, realPart w) andalso Real.==(imagPart z, imagPart w)
        fun !=(z, w) = Real.!=(realPart z, realPart w) andalso Real.!=(imagPart z, imagPart w)
        fun fromInt i = (Real.fromInt i, 0.0)
        fun toString (r,i) =
            String.concat ["(",Real.toString r,",",Real.toString i,")"]
        fun exp (x, y) =
            let val expx = Real.exp x
            in (Real.*(x, (Real.cos y)), Real.*(x, (Real.sin y)))
            end
    local
        val half = Real.recip (Real.fromInt 2)
    in
        fun sqrt (z as (x,y)) =
            if Real.==(x, 0.0) andalso Real.==(y, 0.0) then
                zero
            else
                let val m = Real.+(modulus z, Real.abs x)
                    val u' = Real.sqrt (Real.*(m, half))
                    val v' = Real./(Real.abs y , Real.+(u',u'))
                    val (u,v) = if Real.<(x, 0.0) then (v',u') else (u',v')
                in (u, if Real.<(y, 0.0) then Real.~ v else v)
                end
    end
        fun ln z = (Real.ln (modulus z), arg z)
        fun pow (z, n) =
            let val l = ln z
            in exp (l * n)
            end
        fun sin (x, y) = (Real.*(Real.sin x, Real.cosh y),
                          Real.*(Real.cos x, Real.sinh y))
        fun cos (x, y) = (Real.*(Real.cos x, Real.cosh y),
                          Real.~ (Real.*(Real.sin x, Real.sinh y)))
        fun tan (x, y) =
            let val (sx, cx) = (Real.sin x, Real.cos x)
                val (shy, chy) = (Real.sinh y, Real.cosh y)
                val a = (Real.*(sx, chy), Real.*(cx, shy))
                val b = (Real.*(cx, chy), Real.*(Real.~ sx, shy))
            in a / b
            end
        fun sinh (x, y) = (Real.*(Real.cos y, Real.sinh x),
                           Real.*(Real.sin y, Real.cosh x))
        fun cosh (x, y) = (Real.*(Real.cos y, Real.cosh x),
                           Real.*(Real.sin y, Real.sinh x))
        fun tanh (x, y) =
            let val (sy, cy) = (Real.sin y, Real.cos y)
                val (shx, chx) = (Real.sinh x, Real.cosh x)
                val a = (Real.*(cy, shx), Real.*(sy, chx))
                val b = (Real.*(cy, chx), Real.*(sy, shx))
            in a / b
            end
        fun asin (z as (x,y)) =
            let val w = sqrt (one - z * z)
                val (x',y') = ln ((Real.~ y, x) + w)
            in (y', Real.~ x')
            end
        fun acos (z as (x,y)) = 
            let val (x', y') = sqrt (one + z * z)
                val (x'', y'') = ln (z + (Real.~ y', x'))
            in (y'', Real.~ x'')
            end
        fun atan (z as (x,y)) =
            let val w = sqrt (one + z*z)
                val (x',y') = ln ((Real.-(1.0, y), x) / w)
            in (y', Real.~ x')
            end
        fun atan2 (y, x) = atan(y / x)
        fun asinh x = ln (x + sqrt(one + x * x))
        fun acosh x = ln (x + (x + one) * sqrt((x - one)/(x + one)))
        fun atanh x = ln ((one + x) / sqrt(one - x * x))
        fun scan getc =
            let val scanner = Real.scan getc
            in fn stream => 
                  case scanner stream of
                      NONE => NONE
                    | SOME (a, rest) =>
                      case scanner rest of
                          NONE => NONE
                        | SOME (b, rest) => SOME (make(a,b), rest)
            end
end (* ComplexNumber *)
(*
 Copyright (c) Juan Jose Garcia Ripoll.
 All rights reserved.
 Refer to the COPYRIGHT file for license conditions
*)
structure PrettyPrint :>
    sig
        datatype modifier =
                 Int of int |
                 Real of real |
                 Complex of CNumber.t |
                 String of string
        val list : ('a -> string) -> 'a list  -> unit
        val intList : int list -> unit
        val realList : real list -> unit
        val stringList : string list -> unit
        val array : ('a -> string) -> 'a array -> unit
        val intArray : int array -> unit
        val realArray : real array -> unit
        val stringArray : string array -> unit
        val sequence :
            int -> ((int * 'a -> unit) -> 'b -> unit) -> ('a -> string) -> 'b -> unit
        val print : modifier list -> unit
    end =
struct
    datatype modifier =
             Int of int |
             Real of real |
             Complex of CNumber.t |
             String of string     
    fun list _ [] = print "[]"
      | list cvt (a::resta) =
        let fun loop a [] = (print(cvt a); print "]")
              | loop a (b::restb) = (print(cvt a); print ", "; loop b restb)
        in
            print "[";
            loop a resta
        end
    fun boolList a = list Bool.toString a
    fun intList a = list Int.toString a
    fun realList a = list Real.toString a
    fun stringList a = list (fn x => x) a
    fun array cvt a =
        let val length = Array.length a - 1
            fun print_one (i,x) =
                (print(cvt x); if not(i = length) then print ", " else ())
        in
            Array.appi print_one a
        end
    fun boolArray a = array Bool.toString a
    fun intArray a = array Int.toString a
    fun realArray a = array Real.toString a
    fun stringArray a = array (fn x => x) a
    fun sequence length appi cvt seq =
        let val length = length - 1
            fun print_one (i:int,x) =
                (print(cvt x); if not(i = length) then print ", " else ())
        in
            print "[";
            appi print_one seq;
            print "]\n"
        end
    fun print b =
        let fun printer (Int a) = INumber.toString a
              | printer (Real a) = RNumber.toString a
              | printer (Complex a) = CNumber.toString a
              | printer (String a) = a
        in  List.app (fn x => (TextIO.print (printer x))) b
        end
end (* PrettyPrint *)
fun print' x = List.app print x
(*
 Copyright (c) Juan Jose Garcia Ripoll.
 All rights reserved.
 Refer to the COPYRIGHT file for license conditions
*)
structure INumberArray =
    struct
        open Array
        type array = INumber.t array
        type vector = INumber.t vector
        type elem  = INumber.t
        structure Vector =
            struct
                open Vector
                type vector = INumber.t Vector.vector
                type elem = INumber.t
            end
        fun map f a = tabulate(length a, fn x => (f (sub(a,x))))
        fun mapi f a = tabulate(length a, fn x => (f (x,sub(a,x))))
        fun map2 f a b = tabulate(length a, fn x => (f(sub(a,x),sub(b,x))))
    end
structure RNumberArray =
    struct
        open Real64Array
        val sub = Unsafe.Real64Array.sub
        val update = Unsafe.Real64Array.update
        fun map f a = tabulate(length a, fn x => (f (sub(a,x))))
        fun mapi f a = tabulate(length a, fn x => (f (x,sub(a,x))))
        fun map2 f a b = tabulate(length a, fn x => (f(sub(a,x),sub(b,x))))
    end
(*--------------------- COMPLEX ARRAY -------------------------*)
structure BasicCNumberArray =
struct
        structure Complex : COMPLEX_NUMBER = CNumber
        structure Array : MONO_ARRAY = RNumberArray
        type elem = Complex.t
        type array = Array.array * Array.array
        val maxLen = Array.maxLen
        fun length (a,b) = Array.length a
        fun sub ((a,b),index) = Complex.make(Array.sub(a,index),Array.sub(b,index))
        fun update ((a,b),index,z) =
            let val (re,im) = Complex.split z in
                Array.update(a, index, re);
                Array.update(b, index, im)
            end
    local
        fun makeRange (a, start, NONE) = makeRange(a, start, SOME (length a - 1))
          | makeRange (a, start, SOME last) =
            let val len = length a
                val diff = last - start
            in
                if (start >= len) orelse (last >= len) then
                    raise Subscript
                else if diff < 0 then
                    (a, start, 0)
                else
                    (a, start, diff + 1)
            end
    in
        fun array (size,z:elem) =
            let val realsize = size * 2
                val r = Complex.realPart z
                val i = Complex.imagPart z in
                    (Array.array(size,r), Array.array(size,i))
            end
        fun zeroarray size =
            (Array.array(size,Complex.Real.zero),
             Array.array(size,Complex.Real.zero))
        fun tabulate (size,f) =
            let val a = array(size, Complex.zero)
                fun loop i =
                    case i = size of
                        true => a
                      | false => (update(a, i, f i); loop (i+1))
            in
                loop 0
            end
        fun fromList list =
            let val length = List.length list
                val a = zeroarray length
                fun loop (_, []) = a
                  | loop (i, z::rest) = (update(a, i, z);
                                         loop (i+1, rest))
            in
                loop(0,list)
            end
        fun extract range =
            let val (a, start, len) = makeRange range
                fun copy i = sub(a, i + start)
            in tabulate(len, copy)
            end
        fun concat array_list =
            let val total_length = foldl (op +) 0 (map length array_list)
                val a = array(total_length, Complex.zero)
                fun copy (_, []) = a
                  | copy (pos, v::rest) =
                    let fun loop i =
                        case i = 0 of
                            true => ()
                          | false => (update(a, i+pos, sub(v, i)); loop (i-1))
                    in (loop (length v - 1); copy(length v + pos, rest))
                    end
            in
                copy(0, array_list)
            end
        fun copy {src : array, si : int, len : int option, dst : array, di : int } =
            let val (a, ia, la) = makeRange (src, si, len)
                val (b, ib, lb) = makeRange (dst, di, len)
                fun copy i =
                    case i < 0 of
                        true => ()
                      | false => (update(b, i+ib, sub(a, i+ia)); copy (i-1))
            in copy (la - 1)
            end
        val copyVec = copy
        fun modifyi f range =
            let val (a, start, len) = makeRange range
                val last = start + len
                fun loop i =
                    case i >= last of
                        true => ()
                      | false => (update(a, i, f(i, sub(a,i))); loop (i+1))
            in loop start
            end
        fun modify f a =
            let val last = length a
                fun loop i =
                    case i >= last of
                        true => ()
                      | false => (update(a, i, f(sub(a,i))); loop (i+1))
            in loop 0
            end
        fun app f a =
            let val size = length a
                fun loop i =
                    case i = size of
                        true => ()
                      | false => (f(sub(a,i)); loop (i+1))
            in
                loop 0
            end
        fun appi f range =
            let val (a, start, len) = makeRange range
                val last = start + len
                fun loop i =
                    case i >= last of
                        true => ()
                      | false => (f(i, sub(a,i)); loop (i+1))
            in
                loop start
            end
        fun map f a =
            let val len = length a
                val c = zeroarray len
                fun loop ~1 = c
                  | loop i = (update(a, i, f(sub(a,i))); loop (i-1))
            in loop (len-1)
            end
        fun map2 f a b =
            let val len = length a
                val c = zeroarray len
                fun loop ~1 = c
                  | loop i = (update(c, i, f(sub(a,i),sub(b,i)));
                              loop (i-1))
            in loop (len-1)
            end
        fun mapi f range =
            let val (a, start, len) = makeRange range
                fun rule i = f (i+start, sub(a, i+start))
            in tabulate(len, rule)
            end
        fun foldli f init range =
            let val (a, start, len) = makeRange range
                val last = start + len - 1
                fun loop (i, accum) =
                    case i > last of
                        true => accum
                      | false => loop (i+1, f(i, sub(a,i), accum))
            in loop (start, init)
            end
        fun foldri f init range =
            let val (a, start, len) = makeRange range
                val last = start + len - 1
                fun loop (i, accum) =
                    case i < start of
                        true => accum
                      | false => loop (i-1, f(i, sub(a,i), accum))
            in loop (last, init)
            end
        fun foldl f init a = foldli (fn (_, a, x) => f(a,x)) init (a,0,NONE)
        fun foldr f init a = foldri (fn (_, x, a) => f(x,a)) init (a,0,NONE)
    end
end (* BasicCNumberArray *)
structure CNumberArray =
    struct
        structure Vector =
            struct
                open BasicCNumberArray
                type vector = array
            end : MONO_VECTOR
        type vector = Vector.vector
        open BasicCNumberArray
    end (* CNumberArray *)
structure ITensor =
    struct
        structure Number = INumber
        structure Array = INumberArray
(*
 Copyright (c) Juan Jose Garcia Ripoll.
 All rights reserved.
 Refer to the COPYRIGHT file for license conditions
*)
structure MonoTensor  =
    struct
(* PARAMETERS
        structure Array = Array
*)
        structure Index  = Index
        type elem = Array.elem
        type index = Index.t
        type tensor = {shape : index, indexer : Index.indexer, data : Array.array}
        type t = tensor
        exception Shape
        exception Match
        exception Index
    local
    (*----- LOCALS -----*)
        fun make' (shape, data) =
            {shape = shape, indexer = Index.indexer shape, data = data}
        fun toInt {shape, indexer, data} index = indexer index
        fun splitList (l as (a::rest), place) =
            let fun loop (left,here,right) 0 =  (List.rev left,here,right)
                  | loop (_,_,[]) place = raise Index
                  | loop (left,here,a::right) place = 
                loop (here::left,a,right) (place-1)
            in
                if place <= 0 then
                    loop ([],a,rest) (List.length rest - place)
                else
                    loop ([],a,rest) (place - 1)
            end
    in
    (*----- STRUCTURAL OPERATIONS & QUERIES ------*)
        fun new (shape, init) =
            if not (Index.validShape shape) then
                raise Shape
            else
                let val length = Index.length shape in
                    {shape = shape,
                     indexer = Index.indexer shape,
                     data = Array.array(length,init)}
                end
        fun toArray {shape, indexer, data} = data
        fun length {shape, indexer, data} =  Array.length data
        fun shape {shape, indexer, data} = shape
        fun rank t = List.length (shape t)
        fun reshape new_shape tensor =
            if Index.validShape new_shape then
                case (Index.length new_shape) = length tensor of
                    true => make'(new_shape, toArray tensor)
                  | false => raise Match
            else
                raise Shape
        fun fromArray (s, a) =
            case Index.validShape s andalso 
                 ((Index.length s) = (Array.length a)) of
                 true => make'(s, a)
               | false => raise Shape
        fun fromList (s, a) = fromArray (s, Array.fromList a)
        fun tabulate (shape,f) =
            if Index.validShape shape then
                let val last = Index.last shape
                    val length = Index.length shape
                    val c = Array.array(length, f last)
                    fun dotable (c, indices, i) =
                        (Array.update(c, i, f indices);
                         if i <= 1
                         then c
                         else dotable(c, Index.prev' shape indices, i-1))
                in make'(shape,dotable(c, Index.prev' shape last, length-2))
                end
            else
                raise Shape
        (*----- ELEMENTWISE OPERATIONS -----*)
        fun sub (t, index) = Array.sub(#data t, toInt t index)
        fun update (t, index, value) =
            Array.update(toArray t, toInt t index, value)
        fun map f {shape, indexer, data} =
            {shape = shape, indexer = indexer, data = Array.map f data}
        fun map2 f t1 t2=
            let val {shape=shape1, indexer=indexer1, data=data1} = t1
                val {shape=shape2, indexer=indexer2, data=data2} = t2
            in
                if Index.eq(shape1,shape2) then
                    {shape = shape1,
                     indexer = indexer1,
                     data = Array.map2 f data1 data2}
                else
                    raise Match
        end
        fun appi f tensor = Array.appi f (toArray tensor)
        fun app f tensor = Array.app f (toArray tensor)
        fun all f tensor =
            let val a = toArray tensor
            in Loop.all(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun any f tensor =
            let val a = toArray tensor
            in Loop.any(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun foldl f init tensor = Array.foldl f init (toArray tensor)
        fun foldln f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Array.update(c, ic, f(Array.sub(c,ic), Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
        (* --- POLYMORPHIC ELEMENTWISE OPERATIONS --- *)
        fun array_map' f a =
            let fun apply index = f(Array.sub(a,index)) in
                Tensor.Array.tabulate(Array.length a, apply)
            end
        fun map' f t = Tensor.fromArray(shape t, array_map' f (toArray t))
        fun map2' f t1 t2 =
            let val d1 = toArray t1
                val d2 = toArray t2
                fun apply i = f (Array.sub(d1,i), Array.sub(d2,i))
                val len = Array.length d1
            in
                if Index.eq(shape t1, shape t2) then
                    Tensor.fromArray(shape t1, Tensor.Array.tabulate(len,apply))
                else
                    raise Match
            end
        fun foldl' f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Tensor.Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Tensor.Array.update(c,ic,f(Tensor.Array.sub(c,ic),Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
    end
    end (* MonoTensor *)
        open MonoTensor
    local
        (*
         LEFT INDEX CONTRACTION:
         a = a(i1,i2,...,in)
         b = b(j1,j2,...,jn)
         c = c(i2,...,in,j2,...,jn)
         = sum(a(k,i2,...,jn)*b(k,j2,...jn)) forall k
         MEANINGFUL VARIABLES:
         lk = i1 = j1
         li = i2*...*in
         lj = j2*...*jn
         *)
        fun do_fold_first a b c lk lj li =
            let fun loopk (0, _,  _,  accum) = accum
                  | loopk (k, ia, ib, accum) =
                    let val delta = Number.*(Array.sub(a,ia),Array.sub(b,ib))
                    in loopk (k-1, ia+1, ib+1, Number.+(delta,accum))
                    end
                fun loopj (0, ib, ic) = c
                  | loopj (j, ib, ic) =
                    let fun loopi (0, ia, ic) = ic
                          | loopi (i, ia, ic) =
                        (Array.update(c, ic, loopk(lk, ia, ib, Number.zero));
                         loopi(i-1, ia+lk, ic+1))
                    in
                        loopj(j-1, ib+lk, loopi(li, 0, ic))
                    end
            in loopj(lj, 0, 0)
            end
    in
        fun +* ta tb =
            let val (rank_a,lk::rest_a,a) = (rank ta, shape ta, toArray ta)
                val (rank_b,lk2::rest_b,b) = (rank tb, shape tb, toArray tb)
            in if not(lk = lk2)
               then raise Match
               else let val li = Index.length rest_a
                        val lj = Index.length rest_b
                        val c = Array.array(li*lj,Number.zero)
                    in fromArray(rest_a @ rest_b,
                                 do_fold_first a b c lk li lj)
                    end
            end
    end
    local
        (*
         LAST INDEX CONTRACTION:
         a = a(i1,i2,...,in)
         b = b(j1,j2,...,jn)
         c = c(i2,...,in,j2,...,jn)
         = sum(mult(a(i1,i2,...,k),b(j1,j2,...,k))) forall k
         MEANINGFUL VARIABLES:
         lk = in = jn
         li = i1*...*i(n-1)
         lj = j1*...*j(n-1)
         *)
        fun do_fold_last a b c lk lj li =
            let fun loopi (0, ia, ic, fac) = ()
                  | loopi (i, ia, ic, fac) =
                    let val old = Array.sub(c,ic)
                        val inc = Number.*(Array.sub(a,ia),fac)
                    in
                        Array.update(c,ic,Number.+(old,inc));
                        loopi(i-1, ia+1, ic+1, fac)
                    end
                fun loopj (j, ib, ic) =
                    let fun loopk (0, ia, ib) = ()
                          | loopk (k, ia, ib) =
                            (loopi(li, ia, ic, Array.sub(b,ib));
                             loopk(k-1, ia+li, ib+lj))
                    in case j of
                           0 => c
                         | _ => (loopk(lk, 0, ib);
                                 loopj(j-1, ib+1, ic+li))
                    end (* loopj *)
            in
                loopj(lj, 0, 0)
            end
    in
        fun *+ ta tb  =
            let val (rank_a,shape_a,a) = (rank ta, shape ta, toArray ta)
                val (rank_b,shape_b,b) = (rank tb, shape tb, toArray tb)
                val (lk::rest_a) = List.rev shape_a
                val (lk2::rest_b) = List.rev shape_b
            in if not(lk = lk2)
               then raise Match
               else let val li = Index.length rest_a
                        val lj = Index.length rest_b
                        val c = Array.array(li*lj,Number.zero)
                    in fromArray(List.rev rest_a @ List.rev rest_b,
                                 do_fold_last a b c lk li lj)
                    end
            end
    end
        (* ALGEBRAIC OPERATIONS *)
        infix **
        infix ==
        infix !=
        fun a + b = map2 Number.+ a b
        fun a - b = map2 Number.- a b
        fun a * b = map2 Number.* a b
        fun a ** i = map (fn x => (Number.**(x,i))) a
        fun ~ a = map Number.~ a
        fun abs a = map Number.abs a
        fun signum a = map Number.signum a
        fun a == b = map2' Number.== a b
        fun a != b = map2' Number.!= a b
        fun toString a = raise Domain
        fun fromInt a = new([1], Number.fromInt a)
        (* TENSOR SPECIFIC OPERATIONS *)
        fun *> n = map (fn x => Number.*(n,x))
        fun print t =
            (PrettyPrint.intList (shape t);
             TextIO.print "\n";
             PrettyPrint.sequence (length t) appi Number.toString t)
        fun normInf a =
            let fun accum (y,x) = Number.max(x,Number.abs y)
            in  foldl accum Number.zero a
            end
    end (* NumberTensor *)
structure RTensor =
    struct
        structure Number = RNumber
        structure Array = RNumberArray
(*
 Copyright (c) Juan Jose Garcia Ripoll.
 All rights reserved.
 Refer to the COPYRIGHT file for license conditions
*)
structure MonoTensor  =
    struct
(* PARAMETERS
        structure Array = Array
*)
        structure Index  = Index
        type elem = Array.elem
        type index = Index.t
        type tensor = {shape : index, indexer : Index.indexer, data : Array.array}
        type t = tensor
        exception Shape
        exception Match
        exception Index
    local
    (*----- LOCALS -----*)
        fun make' (shape, data) =
            {shape = shape, indexer = Index.indexer shape, data = data}
        fun toInt {shape, indexer, data} index = indexer index
        fun splitList (l as (a::rest), place) =
            let fun loop (left,here,right) 0 =  (List.rev left,here,right)
                  | loop (_,_,[]) place = raise Index
                  | loop (left,here,a::right) place = 
                loop (here::left,a,right) (place-1)
            in
                if place <= 0 then
                    loop ([],a,rest) (List.length rest - place)
                else
                    loop ([],a,rest) (place - 1)
            end
    in
    (*----- STRUCTURAL OPERATIONS & QUERIES ------*)
        fun new (shape, init) =
            if not (Index.validShape shape) then
                raise Shape
            else
                let val length = Index.length shape in
                    {shape = shape,
                     indexer = Index.indexer shape,
                     data = Array.array(length,init)}
                end
        fun toArray {shape, indexer, data} = data
        fun length {shape, indexer, data} =  Array.length data
        fun shape {shape, indexer, data} = shape
        fun rank t = List.length (shape t)
        fun reshape new_shape tensor =
            if Index.validShape new_shape then
                case (Index.length new_shape) = length tensor of
                    true => make'(new_shape, toArray tensor)
                  | false => raise Match
            else
                raise Shape
        fun fromArray (s, a) =
            case Index.validShape s andalso 
                 ((Index.length s) = (Array.length a)) of
                 true => make'(s, a)
               | false => raise Shape
        fun fromList (s, a) = fromArray (s, Array.fromList a)
        fun tabulate (shape,f) =
            if Index.validShape shape then
                let val last = Index.last shape
                    val length = Index.length shape
                    val c = Array.array(length, f last)
                    fun dotable (c, indices, i) =
                        (Array.update(c, i, f indices);
                         if i <= 1
                         then c
                         else dotable(c, Index.prev' shape indices, i-1))
                in make'(shape,dotable(c, Index.prev' shape last, length-2))
                end
            else
                raise Shape
        (*----- ELEMENTWISE OPERATIONS -----*)
        fun sub (t, index) = Array.sub(#data t, toInt t index)
        fun update (t, index, value) =
            Array.update(toArray t, toInt t index, value)
        fun map f {shape, indexer, data} =
            {shape = shape, indexer = indexer, data = Array.map f data}
        fun map2 f t1 t2=
            let val {shape=shape1, indexer=indexer1, data=data1} = t1
                val {shape=shape2, indexer=indexer2, data=data2} = t2
            in
                if Index.eq(shape1,shape2) then
                    {shape = shape1,
                     indexer = indexer1,
                     data = Array.map2 f data1 data2}
                else
                    raise Match
        end
        fun appi f tensor = Array.appi f (toArray tensor)
        fun app f tensor = Array.app f (toArray tensor)
        fun all f tensor =
            let val a = toArray tensor
            in Loop.all(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun any f tensor =
            let val a = toArray tensor
            in Loop.any(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun foldl f init tensor = Array.foldl f init (toArray tensor)
        fun foldln f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Array.update(c, ic, f(Array.sub(c,ic), Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
        (* --- POLYMORPHIC ELEMENTWISE OPERATIONS --- *)
        fun array_map' f a =
            let fun apply index = f(Array.sub(a,index)) in
                Tensor.Array.tabulate(Array.length a, apply)
            end
        fun map' f t = Tensor.fromArray(shape t, array_map' f (toArray t))
        fun map2' f t1 t2 =
            let val d1 = toArray t1
                val d2 = toArray t2
                fun apply i = f (Array.sub(d1,i), Array.sub(d2,i))
                val len = Array.length d1
            in
                if Index.eq(shape t1, shape t2) then
                    Tensor.fromArray(shape t1, Tensor.Array.tabulate(len,apply))
                else
                    raise Match
            end
        fun foldl' f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Tensor.Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Tensor.Array.update(c,ic,f(Tensor.Array.sub(c,ic),Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
    end
    end (* MonoTensor *)
        open MonoTensor
    local
        (*
         LEFT INDEX CONTRACTION:
         a = a(i1,i2,...,in)
         b = b(j1,j2,...,jn)
         c = c(i2,...,in,j2,...,jn)
         = sum(a(k,i2,...,jn)*b(k,j2,...jn)) forall k
         MEANINGFUL VARIABLES:
         lk = i1 = j1
         li = i2*...*in
         lj = j2*...*jn
         *)
        fun do_fold_first a b c lk lj li =
            let fun loopk (0, _,  _,  accum) = accum
                  | loopk (k, ia, ib, accum) =
                    let val delta = Number.*(Array.sub(a,ia),Array.sub(b,ib))
                    in loopk (k-1, ia+1, ib+1, Number.+(delta,accum))
                    end
                fun loopj (0, ib, ic) = c
                  | loopj (j, ib, ic) =
                    let fun loopi (0, ia, ic) = ic
                          | loopi (i, ia, ic) =
                        (Array.update(c, ic, loopk(lk, ia, ib, Number.zero));
                         loopi(i-1, ia+lk, ic+1))
                    in
                        loopj(j-1, ib+lk, loopi(li, 0, ic))
                    end
            in loopj(lj, 0, 0)
            end
    in
        fun +* ta tb =
            let val (rank_a,lk::rest_a,a) = (rank ta, shape ta, toArray ta)
                val (rank_b,lk2::rest_b,b) = (rank tb, shape tb, toArray tb)
            in if not(lk = lk2)
               then raise Match
               else let val li = Index.length rest_a
                        val lj = Index.length rest_b
                        val c = Array.array(li*lj,Number.zero)
                    in fromArray(rest_a @ rest_b,
                                 do_fold_first a b c lk li lj)
                    end
            end
    end
    local
        (*
         LAST INDEX CONTRACTION:
         a = a(i1,i2,...,in)
         b = b(j1,j2,...,jn)
         c = c(i2,...,in,j2,...,jn)
         = sum(mult(a(i1,i2,...,k),b(j1,j2,...,k))) forall k
         MEANINGFUL VARIABLES:
         lk = in = jn
         li = i1*...*i(n-1)
         lj = j1*...*j(n-1)
         *)
        fun do_fold_last a b c lk lj li =
            let fun loopi (0, ia, ic, fac) = ()
                  | loopi (i, ia, ic, fac) =
                    let val old = Array.sub(c,ic)
                        val inc = Number.*(Array.sub(a,ia),fac)
                    in
                        Array.update(c,ic,Number.+(old,inc));
                        loopi(i-1, ia+1, ic+1, fac)
                    end
                fun loopj (j, ib, ic) =
                    let fun loopk (0, ia, ib) = ()
                          | loopk (k, ia, ib) =
                            (loopi(li, ia, ic, Array.sub(b,ib));
                             loopk(k-1, ia+li, ib+lj))
                    in case j of
                           0 => c
                         | _ => (loopk(lk, 0, ib);
                                 loopj(j-1, ib+1, ic+li))
                    end (* loopj *)
            in
                loopj(lj, 0, 0)
            end
    in
        fun *+ ta tb  =
            let val (rank_a,shape_a,a) = (rank ta, shape ta, toArray ta)
                val (rank_b,shape_b,b) = (rank tb, shape tb, toArray tb)
                val (lk::rest_a) = List.rev shape_a
                val (lk2::rest_b) = List.rev shape_b
            in if not(lk = lk2)
               then raise Match
               else let val li = Index.length rest_a
                        val lj = Index.length rest_b
                        val c = Array.array(li*lj,Number.zero)
                    in fromArray(List.rev rest_a @ List.rev rest_b,
                                 do_fold_last a b c lk li lj)
                    end
            end
    end
        (* ALGEBRAIC OPERATIONS *)
        infix **
        infix ==
        infix !=
        fun a + b = map2 Number.+ a b
        fun a - b = map2 Number.- a b
        fun a * b = map2 Number.* a b
        fun a ** i = map (fn x => (Number.**(x,i))) a
        fun ~ a = map Number.~ a
        fun abs a = map Number.abs a
        fun signum a = map Number.signum a
        fun a == b = map2' Number.== a b
        fun a != b = map2' Number.!= a b
        fun toString a = raise Domain
        fun fromInt a = new([1], Number.fromInt a)
        (* TENSOR SPECIFIC OPERATIONS *)
        fun *> n = map (fn x => Number.*(n,x))
        fun print t =
            (PrettyPrint.intList (shape t);
             TextIO.print "\n";
             PrettyPrint.sequence (length t) appi Number.toString t)
        fun a / b = map2 Number./ a b
        fun recip a = map Number.recip a
        fun ln a = map Number.ln a
        fun pow (a, b) = map (fn x => (Number.pow(x,b))) a
        fun exp a = map Number.exp a
        fun sqrt a = map Number.sqrt a
        fun cos a = map Number.cos a
        fun sin a = map Number.sin a
        fun tan a = map Number.tan a
        fun sinh a = map Number.sinh a
        fun cosh a = map Number.cosh a
        fun tanh a = map Number.tanh a
        fun asin a = map Number.asin a
        fun acos a = map Number.acos a
        fun atan a = map Number.atan a
        fun asinh a = map Number.asinh a
        fun acosh a = map Number.acosh a
        fun atanh a = map Number.atanh a
        fun atan2 (a,b) = map2 Number.atan2 a b
        fun normInf a =
            let fun accum (y,x) = Number.max(x,Number.abs y)
            in  foldl accum Number.zero a
            end
        fun norm1 a =
            let fun accum (y,x) = Number.+(x,Number.abs y)
            in  foldl accum Number.zero a
            end
        fun norm2 a =
            let fun accum (y,x) = Number.+(x, Number.*(y,y))
            in Number.sqrt(foldl accum Number.zero a)
            end
    end (* RTensor *)
structure CTensor =
struct
    structure Number = CNumber
    structure Array = CNumberArray
(*
 Copyright (c) Juan Jose Garcia Ripoll.
 All rights reserved.
 Refer to the COPYRIGHT file for license conditions
*)
structure MonoTensor  =
    struct
(* PARAMETERS
        structure Array = Array
*)
        structure Index  = Index
        type elem = Array.elem
        type index = Index.t
        type tensor = {shape : index, indexer : Index.indexer, data : Array.array}
        type t = tensor
        exception Shape
        exception Match
        exception Index
    local
    (*----- LOCALS -----*)
        fun make' (shape, data) =
            {shape = shape, indexer = Index.indexer shape, data = data}
        fun toInt {shape, indexer, data} index = indexer index
        fun splitList (l as (a::rest), place) =
            let fun loop (left,here,right) 0 =  (List.rev left,here,right)
                  | loop (_,_,[]) place = raise Index
                  | loop (left,here,a::right) place = 
                loop (here::left,a,right) (place-1)
            in
                if place <= 0 then
                    loop ([],a,rest) (List.length rest - place)
                else
                    loop ([],a,rest) (place - 1)
            end
    in
    (*----- STRUCTURAL OPERATIONS & QUERIES ------*)
        fun new (shape, init) =
            if not (Index.validShape shape) then
                raise Shape
            else
                let val length = Index.length shape in
                    {shape = shape,
                     indexer = Index.indexer shape,
                     data = Array.array(length,init)}
                end
        fun toArray {shape, indexer, data} = data
        fun length {shape, indexer, data} =  Array.length data
        fun shape {shape, indexer, data} = shape
        fun rank t = List.length (shape t)
        fun reshape new_shape tensor =
            if Index.validShape new_shape then
                case (Index.length new_shape) = length tensor of
                    true => make'(new_shape, toArray tensor)
                  | false => raise Match
            else
                raise Shape
        fun fromArray (s, a) =
            case Index.validShape s andalso 
                 ((Index.length s) = (Array.length a)) of
                 true => make'(s, a)
               | false => raise Shape
        fun fromList (s, a) = fromArray (s, Array.fromList a)
        fun tabulate (shape,f) =
            if Index.validShape shape then
                let val last = Index.last shape
                    val length = Index.length shape
                    val c = Array.array(length, f last)
                    fun dotable (c, indices, i) =
                        (Array.update(c, i, f indices);
                         if i <= 1
                         then c
                         else dotable(c, Index.prev' shape indices, i-1))
                in make'(shape,dotable(c, Index.prev' shape last, length-2))
                end
            else
                raise Shape
        (*----- ELEMENTWISE OPERATIONS -----*)
        fun sub (t, index) = Array.sub(#data t, toInt t index)
        fun update (t, index, value) =
            Array.update(toArray t, toInt t index, value)
        fun map f {shape, indexer, data} =
            {shape = shape, indexer = indexer, data = Array.map f data}
        fun map2 f t1 t2=
            let val {shape=shape1, indexer=indexer1, data=data1} = t1
                val {shape=shape2, indexer=indexer2, data=data2} = t2
            in
                if Index.eq(shape1,shape2) then
                    {shape = shape1,
                     indexer = indexer1,
                     data = Array.map2 f data1 data2}
                else
                    raise Match
        end
        fun appi f tensor = Array.appi f (toArray tensor, 0, NONE)
        fun app f tensor = Array.app f (toArray tensor)
        fun all f tensor =
            let val a = toArray tensor
            in Loop.all(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun any f tensor =
            let val a = toArray tensor
            in Loop.any(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun foldl f init tensor = Array.foldl f init (toArray tensor)
        fun foldln f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Array.update(c, ic, f(Array.sub(c,ic), Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
        (* --- POLYMORPHIC ELEMENTWISE OPERATIONS --- *)
        fun array_map' f a =
            let fun apply index = f(Array.sub(a,index)) in
                Tensor.Array.tabulate(Array.length a, apply)
            end
        fun map' f t = Tensor.fromArray(shape t, array_map' f (toArray t))
        fun map2' f t1 t2 =
            let val d1 = toArray t1
                val d2 = toArray t2
                fun apply i = f (Array.sub(d1,i), Array.sub(d2,i))
                val len = Array.length d1
            in
                if Index.eq(shape t1, shape t2) then
                    Tensor.fromArray(shape t1, Tensor.Array.tabulate(len,apply))
                else
                    raise Match
            end
        fun foldl' f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Tensor.Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Tensor.Array.update(c,ic,f(Tensor.Array.sub(c,ic),Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
    end
    end (* MonoTensor *)
    open MonoTensor
    local
        (*
         LEFT INDEX CONTRACTION:
         a = a(i1,i2,...,in)
         b = b(j1,j2,...,jn)
         c = c(i2,...,in,j2,...,jn)
         = sum(a(k,i2,...,jn)*b(k,j2,...jn)) forall k
         MEANINGFUL VARIABLES:
         lk = i1 = j1
         li = i2*...*in
         lj = j2*...*jn
         *)
        fun do_fold_first a b c lk lj li =
            let fun loopk (0, _, _, r, i) = Number.make(r,i)
                  | loopk (k, ia, ib, r, i) =
                    let val (ar, ai) = Array.sub(a,ia)
                        val (br, bi) = Array.sub(b,ib)
                        val dr = ar * br - ai * bi
                        val di = ar * bi + ai * br
                    in loopk (k-1, ia+1, ib+1, r+dr, i+di)
                    end
                fun loopj (0, ib, ic) = c
                  | loopj (j, ib, ic) =
                    let fun loopi (0, ia, ic) = ic
                          | loopi (i, ia, ic) =
                            (Array.update(c, ic, loopk(lk, ia, ib, RNumber.zero, RNumber.zero));
                             loopi(i-1, ia+lk, ic+1))
                    in loopj(j-1, ib+lk, loopi(li, 0, ic))
                    end
            in loopj(lj, 0, 0)
            end
    in
        fun +* ta tb =
            let val (rank_a,lk::rest_a,a) = (rank ta, shape ta, toArray ta)
                val (rank_b,lk2::rest_b,b) = (rank tb, shape tb, toArray tb)
            in if not(lk = lk2)
               then raise Match
               else let val li = Index.length rest_a
                        val lj = Index.length rest_b
                        val c = Array.array(li*lj,Number.zero)
                    in fromArray(rest_a @ rest_b, do_fold_first a b c lk li lj)
                    end
            end
    end
    local
        (*
         LAST INDEX CONTRACTION:
         a = a(i1,i2,...,in)
         b = b(j1,j2,...,jn)
         c = c(i2,...,in,j2,...,jn)
         = sum(mult(a(i1,i2,...,k),b(j1,j2,...,k))) forall k
         MEANINGFUL VARIABLES:
         lk = in = jn
         li = i1*...*i(n-1)
         lj = j1*...*j(n-1)
         *)
        fun do_fold_last a b c lk lj li =
            let fun loopi(0, _, _, _, _) = ()
                  | loopi(i, ia, ic, br, bi) =
                    let val (cr,ci) = Array.sub(c,ic)
                        val (ar,ai) = Array.sub(a,ia)
                        val dr = (ar * br - ai * bi)
                        val di = (ar * bi + ai * br)
                    in
                        Array.update(c,ic,Number.make(cr+dr,ci+di));
                        loopi(i-1, ia+1, ic+1, br, bi)
                    end
                fun loopj(j, ib, ic) =
                    let fun loopk(0, _, _) = ()
                          | loopk(k, ia, ib) =
                            let val (br, bi) = Array.sub(b,ib)
                            in
                                loopi(li, ia, ic, br, bi);
                                loopk(k-1, ia+li, ib+lj)
                            end
                in case j of
                    0 => c
                  | _ => (loopk(lk, 0, ib);
                          loopj(j-1, ib+1, ic+li))
                end (* loopj *)
            in
                loopj(lj, 0, 0)
            end
    in
        fun *+ ta tb  =
            let val (rank_a,shape_a,a) = (rank ta, shape ta, toArray ta)
                val (rank_b,shape_b,b) = (rank tb, shape tb, toArray tb)
                val (lk::rest_a) = List.rev shape_a
                val (lk2::rest_b) = List.rev shape_b
            in
                if not(lk = lk2) then
                    raise Match
                else
                    let val li = Index.length rest_a
                        val lj = Index.length rest_b
                        val c = Array.array(li*lj,Number.zero)
                    in
                        fromArray(List.rev rest_a @ List.rev rest_b,
                                  do_fold_last a b c lk li lj)
                    end
            end
    end
    (* ALGEBRAIC OPERATIONS *)
    infix **
    infix ==
    infix !=
    fun a + b = map2 Number.+ a b
    fun a - b = map2 Number.- a b
    fun a * b = map2 Number.* a b
    fun a ** i = map (fn x => (Number.**(x,i))) a
    fun ~ a = map Number.~ a
    fun abs a = map Number.abs a
    fun signum a = map Number.signum a
    fun a == b = map2' Number.== a b
    fun a != b = map2' Number.!= a b
    fun toString a = raise Domain
    fun fromInt a = new([1], Number.fromInt a)
    (* TENSOR SPECIFIC OPERATIONS *)
    fun *> n = map (fn x => Number.*(n,x))
    fun print t =
        (PrettyPrint.intList (shape t);
         TextIO.print "\n";
         PrettyPrint.sequence (length t) appi Number.toString t)
    fun a / b = map2 Number./ a b
    fun recip a = map Number.recip a
    fun ln a = map Number.ln a
    fun pow (a, b) = map (fn x => (Number.pow(x,b))) a
    fun exp a = map Number.exp a
    fun sqrt a = map Number.sqrt a
    fun cos a = map Number.cos a
    fun sin a = map Number.sin a
    fun tan a = map Number.tan a
    fun sinh a = map Number.sinh a
    fun cosh a = map Number.cosh a
    fun tanh a = map Number.tanh a
    fun asin a = map Number.asin a
    fun acos a = map Number.acos a
    fun atan a = map Number.atan a
    fun asinh a = map Number.asinh a
    fun acosh a = map Number.acosh a
    fun atanh a = map Number.atanh a
    fun atan2 (a,b) = map2 Number.atan2 a b
    fun normInf a =
        let fun accum (y,x) = RNumber.max(x, Number.realPart(Number.abs y))
        in  foldl accum RNumber.zero a
        end
    fun norm1 a =
        let fun accum (y,x) = RNumber.+(x, Number.realPart(Number.abs y))
        in  foldl accum RNumber.zero a
        end
    fun norm2 a =
        let fun accum (y,x) = RNumber.+(x, Number.abs2 y)
        in RNumber.sqrt(foldl accum RNumber.zero a)
        end
end (* CTensor *)
structure MathFile =
struct

type file = TextIO.instream

exception Data

fun assert NONE = raise Data
  | assert (SOME a) = a

(* ------------------ INPUT --------------------- *)

fun intRead file = assert(TextIO.scanStream INumber.scan file)
fun realRead file = assert(TextIO.scanStream RNumber.scan file)
fun complexRead file = assert(TextIO.scanStream CNumber.scan file)

fun listRead eltScan file =
    let val length = intRead file
        fun eltRead file = assert(TextIO.scanStream eltScan file)
        fun loop (0,accum) = accum
          | loop (i,accum) = loop(i-1, eltRead file :: accum)
    in
        if length < 0
        then raise Data
        else List.rev(loop(length,[]))
    end

fun intListRead file = listRead INumber.scan file
fun realListRead file = listRead RNumber.scan file
fun complexListRead file = listRead CNumber.scan file

fun intTensorRead file =
    let val shape = intListRead file
        val length = Index.length shape
        val first = intRead file
        val a = ITensor.Array.array(length, first)
        fun loop 0 = ITensor.fromArray(shape, a)
          | loop j = (ITensor.Array.update(a, length-j, intRead file);
                      loop (j-1))
    in loop (length - 1)
    end

fun realTensorRead file =
    let val shape = intListRead file
        val length = Index.length shape
        val first = realRead file
        val a = RTensor.Array.array(length, first)
        fun loop 0 = RTensor.fromArray(shape, a)
          | loop j = (RTensor.Array.update(a, length-j, realRead file);
                      loop (j-1))
    in loop (length - 1)
    end

fun complexTensorRead file =
    let val shape = intListRead file
        val length = Index.length shape
        val first = complexRead file
        val a = CTensor.Array.array(length, first)
        fun loop j = if j = length
                     then CTensor.fromArray(shape, a)
                     else (CTensor.Array.update(a, j, complexRead file);
                           loop (j+1))
    in loop 1
    end

(* ------------------ OUTPUT -------------------- *)
fun linedOutput(file, x) = (TextIO.output(file, x); TextIO.output(file, "\n"))

fun intWrite file x = linedOutput(file, INumber.toString x)
fun realWrite file x = linedOutput(file, RNumber.toString x)
fun complexWrite file x =
    let val (r,i) = CNumber.split x
    in linedOutput(file, concat [RNumber.toString r, " ", RNumber.toString i])
    end

fun listWrite converter file x =
    (intWrite file (length x);
     List.app (fn x => (linedOutput(file, converter x))) x)

fun intListWrite file x = listWrite INumber.toString file x
fun realListWrite file x = listWrite RNumber.toString file x
fun complexListWrite file x = listWrite CNumber.toString file x

fun intTensorWrite file x = (intListWrite file (ITensor.shape x); ITensor.app (fn x => (intWrite file x)) x)
fun realTensorWrite file x = (intListWrite file (RTensor.shape x); RTensor.app (fn x => (realWrite file x)) x)
fun complexTensorWrite file x = (intListWrite file (CTensor.shape x); CTensor.app (fn x => (complexWrite file x)) x)
end

fun loop 0 _ = ()
  | loop n f = (f(); loop (n-1) f)

fun test_operator new list_op list_sizes =
    let fun test_many list_op size =
        let fun test_op (times,f) =
            let val a = new size
            in (EvalTimer.timerOn();
                loop times (fn _ => f(a,a));
                let val t = LargeInt.toInt(EvalTimer.timerRead()) div times
                    val i = StringCvt.padLeft #" " 6 (Int.toString t)
                in print i
                end)
            end
        in
            print (Int.toString size);
            print " ";
            List.app test_op list_op;
            print "\n"
        end
    in List.app (test_many list_op) list_sizes
    end

structure Main =
   struct
      fun one() =
         let
            val _ =
               let val operators = [(20, RTensor.+), (20, RTensor.* ), (20, RTensor./),
                                    (4, fn (a,b) => RTensor.+* a b),
                                    (4, fn (a,b) => RTensor.*+ a b)]
                  fun constructor size = RTensor.new([size,size],1.0)
               in
                  print "Real tensors: (+, *, /, +*, *+)\n";
                  test_operator constructor operators [100,200,300,400,500];
                  print "\n\n"
               end
            
      val _ =
         let val operators = [(20, CTensor.+), (20, CTensor.* ), (20, CTensor./),
                              (4, fn (a,b) => CTensor.+* a b),
                              (4, fn (a,b) => CTensor.*+ a b)]
            fun constructor size = CTensor.new([size,size],CNumber.one)
         in
            print "Real tensors: (+, *, /, +*, *+)\n";
            test_operator constructor operators [100,200,300,400,500];
            print "\n\n"
         end
         in ()
         end

      fun doit n =
         if n = 0
            then ()
         else (one ()
               ; doit (n - 1))
   end
