functor Vector (S: sig
		      include VECTOR_STRUCTS
		      val unsafeSub: 'a t * int -> 'a
		   end): VECTOR =
struct

open S

fun unfold (n, a, f) = unfoldi (n, a, f o #2)
   
fun tabulate (n, f) = unfoldi (n, (), fn (i, ()) => (f i, ()))

datatype ('a, 'b) continue =
   Continue of 'a
  | Done of 'b

fun fold' (v, start, b, f, g) =
   let
      val n = length v
      fun loop (i, b) =
	 if i = n
	    then g b
	 else
	    case f (i, unsafeSub (v, i), b) of
	       Continue b => loop (i + 1, b)
	     | Done c => c
   in
      if 0 <= start andalso start <= n
	 then loop (start, b)
      else Error.bug "Vector.fold'"
   end

fun foldFrom (v, start, b, f) =
   fold' (v, start, b,
	  fn (_, a, b) => Continue (f (a, b)),
	  fn b => b)

fun fold (a, b, f) = foldFrom (a, 0, b, f)

local
   structure F = Fold (type 'a t = 'a t
		       type 'a elt = 'a
		       val fold = fold)
   open F
in
   val peekMapi = peekMapi
end

fun isEmpty a = 0 = length a

fun new (n, x) = tabulate (n, fn _ => x)
   
fun mapi (a, f) = tabulate (length a, fn i => f (i, unsafeSub (a, i)))

fun map (v, f) = mapi (v, f o #2)

fun copy v = map (v, fn x => x)

fun existsR (v, start, stop, f) =
   fold' (v, start, (),
	  fn (i, a, ()) => if i = stop
			      then Done false
			   else if f a
				   then Done true
				else Continue (),
	  fn _ => false)

fun foldi (v, b, f) = fold' (v, 0, b, Continue o f, fn b => b)

fun loopi (v, f, g) =
   fold' (v, 0, (),
	  fn (i, a, ()) => (case f (i, a) of
			       NONE => Continue ()
			     | SOME b => Done b),
	  g)

fun loop (v, f, g) = loopi (v, f o #2, g)

fun peekMap (v, f) =
   loop (v,
	 fn a => (case f a of
		     NONE => NONE
		   | z => SOME z),
	 fn () => NONE)

fun fromListMap (l, f) =
   let
      val r = ref l
   in
      tabulate (List.length l, fn _ =>
		case !r of
		   [] => Error.bug "Vector.fromListMap"
		 | x :: l => (r := l; f x))
   end

fun fromList l = fromListMap (l, fn x => x)

fun foldr2 (a, a', b, f) =
   let
      val n = length a
      val n' = length a'
      fun loop (i, b) =
	 if i < 0
	    then b
	 else loop (i - 1, f (unsafeSub (a, i), unsafeSub (a', i), b))
   in
      if n = n'
	 then loop (n - 1, b)
      else raise Fail "Vector.foldr2"
   end
   
fun fold2From (a, a', start, b, f) =
   let
      val n = length a
      val n' = length a'
      fun loop (i, b) =
	 if i = n
	    then b
	 else loop (i + 1, f (unsafeSub (a, i), unsafeSub (a', i), b))
   in
      if n = n' andalso 0 <= start andalso start <= n 
	 then loop (start, b)
      else Error.bug "Vector.fold2"
   end

fun fold2 (a, a', b, f) = fold2From (a, a', 0, b, f)
   
fun fold3From (a, a', a'', start, b, f) =
   let
      val n = length a
      val n' = length a'
      val n'' = length a''
      fun loop (i, b) =
	 if i = n
	    then b
	 else loop (i + 1, f (unsafeSub (a, i),
			      unsafeSub (a', i),
			      unsafeSub (a'', i),
			      b))
   in
      if n = n' andalso n = n'' andalso 0 <= start andalso start <= n
	 then loop (start, b)
      else Error.bug "Vector.fold3"
   end

fun fold3 (a, a', a'', b, f) = fold3From (a, a', a'', 0, b, f)

fun foreachR (v, start, stop, f) =
   if 0 <= start andalso start <= stop andalso stop <= length v
      
      then
	 let
	    fun step (i, a, ()) =
	       if i = stop
		  then Done ()
	       else (f a; Continue ())
	 in
	    fold' (v, start, (), step, fn () => ())
	 end
   else Error.bug "Vector.foreachR"

fun foreach2 (a, a', f) =
   fold2 (a, a', (), fn (x, x', ()) => f (x, x'))

fun forall2 (a, a', f) =
   DynamicWind.withEscape
   (fn escape =>
    (foreach2 (a, a', fn (a, a') => if f (a, a') then () else escape false)
     ; true))

fun foreachi (a, f) = foldi (a, (), fn (i, x, ()) => f (i, x))

fun foreach (a, f) = foreachi (a, f o #2)

fun 'a peeki (a, f) =
   DynamicWind.withEscape
   (fn escape => 
    (foreachi (a, fn (i, x) => if f (i, x) then escape (SOME (i, x)) else ())
     ; NONE))

fun peek (a, f) = Option.map (peeki (a, f o #2), #2)

fun existsi (a, f) = isSome (peeki (a, f))

fun exists (a, f) = existsi (a, f o #2)

fun contains (v, a, f) = exists (v, fn a' => f (a, a'))

fun foralli (a, f) = not (existsi (a, not o f))

fun forall (a, f) = foralli (a, f o #2)

fun equals (a, a', equals) =
   length a = length a'
   andalso foralli (a, fn (i, x) => equals (x, unsafeSub (a', i)))

fun foldri (a, b, f) =
   Int.foldDown (0, length a, b, fn (i, b) => f (i, unsafeSub (a, i), b))

fun foldr (a, b, f) =
   foldri (a, b, fn (i, a, b) => f (a, b))
   
fun foreachri (a, f) = foldri (a, (), fn (i, x, ()) => f (i, x))

fun foreachr (a, f) = foreachri (a, f o #2)

fun toList a = foldr (a, [], op ::)

fun toListMap (a, f) = foldr (a, [], fn (a, ac) => f a :: ac)

fun layout l v = Layout.tuple (toListMap (v, l))

fun maxIndex a = Int.sub1 (length a)

fun new0 () = tabulate (0, fn _ => Error.bug "Vector.new0")

fun new1 x = tabulate (1, fn _ => x)

fun new2 (x0, x1) = tabulate (2, fn 0 => x0 | 1 => x1 | _ => raise Fail "new2")

fun new3 (x0, x1, x2) =
   tabulate (3,
	     fn 0 => x0
	      | 1 => x1
	      | 2 => x2
	      | _ => raise Fail "new3")

fun new4 (x0, x1, x2, x3) =
   tabulate (4,
	     fn 0 => x0
	      | 1 => x1
	      | 2 => x2
	      | 3 => x3
	      | _ => raise Fail "new4")

fun unzip (a: ('a * 'b) t) = (map (a, #1), map (a, #2))

fun unzip3 (a: ('a * 'b * 'c) t) = (map (a, #1), map (a, #2), map (a, #3))

fun rev v =
   let
      val n = length v
      val n1 = n - 1
   in
      tabulate (n, fn i => unsafeSub (v, n1 - i))
   end

fun fromListRev l = rev (fromList l)

fun mapAndFold (v, b, f) =
   let
      val r = ref b
      val v = map (v, fn x =>
		   let
		      val (c, b) = f (x, !r)
		      val _ = r := b
		   in c
		   end)
   in (v, !r)
   end

fun map2i (v, v', f) =
   let
      val n = length v
   in
      if n = length v'
	 then tabulate (n, fn i => f (i, unsafeSub (v, i), unsafeSub (v', i)))
      else Error.bug "Vector.map2"
   end

fun map2 (v, v', f) = map2i (v, v', fn (_, x, x') => f (x, x'))

fun map2AndFold (v, v', b, f) =
   let
      val r = ref b
      val v =
	 map2 (v, v', fn (x, x') =>
	       let
		  val (y, b) = f (x, x', !r)
		  val _ = r := b
	       in y
	       end)
   in (v, !r)
   end
		    
fun map3 (v1, v2, v3, f) =
   let
      val n = length v1
   in
      if n = length v2 andalso n = length v3
	 then tabulate (n, fn i => f (unsafeSub (v1, i),
				      unsafeSub (v2, i),
				      unsafeSub (v3, i)))
      else Error.bug "Vector.map3"
   end

fun zip (v, v') = map2 (v, v', fn z => z)

local
   fun doit (f, mapi) =
      let
	 val n = ref 0
	 val b = mapi (fn x =>
		       let
			  val b = f x
			  val _ = if isSome b then n := 1 + !n else ()
		       in b
		       end)
	 val r = ref 0
	 fun loop (i: int) =
	    case unsafeSub (b, i) of
	       NONE => loop (i + 1)
	     | SOME b => (r := i + 1; b)
      in tabulate (!n, fn _ => loop (!r))
      end
in
   fun keepAllMapi (a, f) = doit (f, fn f => mapi (a, f))
   fun keepAllMap2i (a, b, f) = doit (f, fn f => map2i (a, b, f))
end

fun keepAllMap (v, f) = keepAllMapi (v, f o #2)

fun keepAllMap2 (v, v', f) = keepAllMap2i (v, v', fn (_, x, x') => f (x, x'))

fun keepAllSome v = keepAllMap (v, fn a => a)

fun keepAll (v, f) = keepAllMap (v, fn a => if f a then SOME a else NONE)

fun compare (v, v', comp) =
   let
      val n = length v
      val n' = length v'
      fun loop i =
	 if i = n
	    then
	       if i = n'
		  then EQUAL
	       else LESS
	 else if i = n'
		 then GREATER
	      else
		 case comp (unsafeSub (v, i), unsafeSub (v', i)) of
		    EQUAL => loop (i + 1)
		  | r => r
   in loop 0
   end

fun toListRev v = fold (v, [], op ::)

fun last v =
   let
      val n = length v
   in
      if n = 0
	 then Error.bug "Vector.last"
      else unsafeSub (v, n - 1)
   end

fun tabulator (n: int, f: ('a -> unit) -> unit) =
   let
      val a = Pervasive.Array.array (n, NONE)
      val r = ref 0
      val _ =
	 f (fn x =>
	    let
	       val i = !r
	    in
	       if i >= n
		  then Error.bug "Vector.tabulator: too many elements"
	       else (Pervasive.Array.update (a, i, SOME x)
		     ; r := i + 1)
	    end)
   in
      if !r < n
	 then Error.bug "Vector.tabulator: not enough elements"
      else tabulate (n, fn i => valOf (Pervasive.Array.sub (a, i)))
   end

fun 'a concat (vs: 'a t list): 'a t =
   case vs of
      [] => new0 ()
    | v :: vs' => 
	 let
	    val n = List.fold (vs, 0, fn (v, s) => s + length v)
	 in
	    unfold (n, (0, v, vs'),
		    let
		       fun loop (i, v, vs) =
			  if i < length v
			     then (sub (v, i), (i + 1, v, vs))
			  else
			     case vs of
				[] => Error.bug "concat"
			      | v :: vs => loop (0, v, vs)
		    in loop
		    end)
	 end

fun concatV vs =
   if 0 = length vs
      then new0 ()
   else
      let
	 val n = fold (vs, 0, fn (v, s) => s + length v)
	 fun state i = (i, sub (vs, i), 0)
      in
	 unfold (n, state 0,
		 let
		    fun loop (i, v, j) =
		       if j < length v
			  then (sub (v, j), (i, v, j + 1))
		       else loop (state (i + 1))
		 in loop
		 end)
   end

fun splitLast v =
   let
      val n = length v
   in
      if 0 = n
	 then Error.bug "splitLast"
      else (tabulate (n - 1, fn i => unsafeSub (v, i)),
	    unsafeSub (v, n - 1))
   end

fun isSortedRange (v: 'a t,
		   start: int,
		   stop: int,
		   le : 'a * 'a -> bool): bool =
   (Assert.assert
    ("isSortedRange", fn () =>
     0 <= start andalso start <= stop andalso stop <= length v)
    ; start = stop
      orelse
      let
	 fun loop (i, prev) =
	    i >= stop
	    orelse let val cur = sub (v, i)
		   in
		      le (prev, cur)
		      andalso loop (i + 1, cur)
		   end
      in loop (start + 1, sub (v, start))
      end)

fun isSorted (v, op <=) = isSortedRange (v, 0, length v, op <=)

fun merge (v, v', op <=) =
   let
      val _ = Assert.assert ("Vector.merge pre", fn () =>
			     isSorted (v, op <=) andalso isSorted (v', op <=))
      val n = length v
      val n' = length v'
      val r = ref 0
      val r' = ref 0
      fun next _ =
	 let
	    val i = !r
	    val i' = !r'
	    (* 0 <= i <= n andalso 0 <= i' <= n' *)
	 in
	    if i = n
	       then
		  let
		     val res = sub (v', i')
		     val _ = Int.inc r'
		  in res
		  end
	    else if i' = n'
		    then 
		       let
			  val res = sub (v, i)
			  val _ = Int.inc r
		       in res
		       end
		 else
		    let
		       val a = sub (v, i)
		       val a' = sub (v', i')
		    in
		       if a <= a'
			  then (Int.inc r; a)
		       else (Int.inc r'; a')
		    end
	 end
      val v = tabulate (n + n', fn _ => next ())
      val _ = Assert.assert ("Vector.merge post", fn () =>
			     isSorted (v, op <=))
   in
      v
   end

fun sort (v, op <=) =
   let
      fun loop v =
	 if isSorted (v, op <=)
	    then v
	 else
	    let
	       val n = length v
	       val m = n div 2
	       val m' = n - m
	       fun get (m, start) =
		  loop
		  (tabulate (m, 
			     let val r = ref start
			     in fn _ =>
				let
				   val i = !r
				   val res = sub (v, i)
				   val _ = r := 2 + i
				in res
				end
			     end))
	    in merge (get (m', 0), get (m, 1), op <=)
	    end
      val v = loop v
      val _ = Assert.assert ("Vector.sort", fn () => isSorted (v, op <=))
   in
      v
   end

fun indexi (v, f) =
   fold' (v, 0, (),
	  fn (i, a, _) => if f (i, a) then Done (SOME i) else Continue (),
	  fn _ => NONE)

fun index (v, f) = indexi (v, f o #2)

fun numTrue (v: 'a t, f: 'a -> bool): int =
   fold (v, 0, fn (a, n) => if f a then n + 1 else n)

fun indices (a: bool t): int t =
   keepAllMapi (a, fn (i, b) => if b then SOME i else NONE)

val indices =
   Trace.trace ("indices", layout Bool.layout, layout Int.layout)
   indices

end
