functor Vector
   (V: sig
	  type 'a vector
	  type 'a elem
	  val maxLen: Int31.int 
	  val tabulate: Int31.int * (Int31.int -> 'a elem) -> 'a vector 
	  val length: 'a vector -> Int31.int 
	  val sub: ('a vector * Int31.int) -> 'a elem
	  val extract: ('a vector * Int31.int * Int31.int option) -> 'a vector 
	  val mapi:
	     ((Int31.int * 'a elem) -> 'b elem)
	     -> ('a vector * Int31.int * Int31.int option) -> 'b vector 
	  val appi:
	     ((Int31.int * 'a elem) -> unit)
	     -> ('a vector * Int31.int * Int31.int option) -> unit 
	  val foldli :
	     ((Int31.int * 'a elem * 'b) -> 'b)
	     -> 'b -> ('a vector * Int31.int * Int31.int option) -> 'b 
	  val foldri :
	     ((Int31.int * 'a elem * 'b) -> 'b)
	     -> 'b -> ('a vector * Int31.int * Int31.int option) -> 'b 
       end) =
   struct
      open V OpenInt32

      val maxLen = fromInt maxLen
      fun tabulate (n, f) = V.tabulate (toInt n, f o fromInt)
      fun length (v: 'a vector) = fromInt (V.length v)
      fun sub (v, i) = V.sub (v, toInt i)
      fun convertSlice (v: 'a vector, i, io) = (v, toInt i, toIntOpt io)
      fun extract z = V.extract (convertSlice z)
      local
	 fun make f g s = f (fn (i, e) => g (fromInt i, e)) (convertSlice s)
      in val mapi = fn z => make mapi z
	 val appi = fn z => make appi z
      end
      local
	 fun make fold f a s =
	    fold (fn (i, e, a) => f (fromInt i, e, a)) a (convertSlice s)
      in
	 val foldli = fn z => make foldli z
	 val foldri = fn z => make foldri z
      end
   end

structure Vector =
   let
      structure V = Vector (open Pervasive.Vector
			    type 'a elem = 'a)
   in struct open Vector V end
   end

functor MonoVector (V: MONO_VECTOR) =
   struct
      structure V' = Vector (open V
			     type 'a vector = vector
			     type 'a elem = elem
			     (* These rebindings are because of an SML/NJ bug. *)
			     val appi = appi
			     val extract = extract
			     val length = length
			     val mapi = mapi
			     val sub = sub
			     val tabulate = tabulate)
      open V V'
      local open V
      in type vector = vector
	 type elem = elem
      end
   end

structure CharVector = MonoVector (CharVector)
structure RealVector = MonoVector (RealVector)
structure Word8Vector = MonoVector (Word8Vector)
