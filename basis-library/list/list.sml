(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure List: LIST =
  struct
     open Primitive.Int
	
     datatype list = datatype list

     exception Empty

     val null =
	fn [] => true
	 | _ => false

     val hd =
	fn x :: _ => x
	 | _ => raise Empty

     val tl =
	fn _ :: l => l
	 | _ => raise Empty

     val rec last =
	fn [] => raise Empty
	 | [x] => x
	 | _ :: l => last l

     val getItem =
	fn [] => NONE
	 | x :: r => SOME (x, r)

     fun nth (l, n) =
	let
	   val rec loop =
	      fn (e :: _, 0) => e
	       | ([], _) => raise Subscript
	       | (_ :: l, n) => loop (l, n -? 1)
	in
	   if not Primitive.safe orelse n >= 0
	      then loop (l, n)
	   else raise Subscript
	end

     fun take (l, n) =
	let
	   val rec loop =
	      fn (l, 0) => []
	       | ([], _) => raise Subscript
	       | (x :: l, n) => x :: loop (l, n -? 1)
	in
	   if not Primitive.safe orelse n >= 0
	      then loop (l, n)
	   else raise Subscript
	end

     fun drop (l, n) =
	let
	   val rec loop =
	      fn (l, 0) => l
	       | ([], _) => raise Subscript
	       | ((_ :: t), n) => loop (t, n -? 1)
	in
	   if not Primitive.safe orelse n >= 0
	      then loop (l, n)
	   else raise Subscript
	end
     
     fun foldl f b l =
	let
	   fun loop (l, b) =
	      case l of
		 [] => b
	       | x :: l => loop (l, f (x, b))
	in loop (l, b)
	end

     fun length l = foldl (fn (_, n) => n +? 1) 0 l
     
     fun appendRev (l1, l2) = foldl (op ::) l2 l1

     val revAppend = appendRev

     fun rev l = appendRev (l, [])

     fun l1 @ l2 =
	case l2 of
	   [] => l1
	 | _ => appendRev (rev l1, l2)

     fun foldr f b l = foldl f b (rev l)

     fun concat ls = foldr (op @) [] ls

     fun app f = foldl (f o #1) ()

     val _ = app (fn x: int => ()) [1, 2, 3]

     fun map f l = rev (foldl (fn (x, l) => f x :: l) [] l)

     fun mapPartial pred l =
	rev (foldl (fn (x, l) => (case pred x of
				   NONE => l
				 | SOME y => y :: l))
		   [] l)

     fun filter pred = mapPartial (fn x => if pred x then SOME x else NONE)

     fun partition pred l =
	let
	   val (pos, neg) =
	      foldl (fn (x, (trues, falses)) =>
		     if pred x then (x :: trues, falses)
		     else (trues, x :: falses))
	      ([], []) l
	in (rev pos, rev neg)
	end

     fun find pred =
	let
	   val rec loop =
	      fn [] => NONE
	       | x :: l => if pred x
			    then SOME x
			 else loop l
	in loop
	end
     
     fun exists pred l =
	case find pred l of
	   NONE => false
	 | SOME _ => true
     
     fun all pred = not o (exists (not o pred))

     fun tabulate (n, f) = 
	if Primitive.safe andalso n < 0
	   then raise Size
	else let
		fun loop (i, ac) = if i >= n then rev ac
				   else loop (i + 1, f i :: ac)
	     in loop (0, [])
	     end
  end

structure ListGlobal: LIST_GLOBAL = List
open ListGlobal
