(* Copyright (C) 2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor Switch (S: SWITCH_STRUCTS): SWITCH =
struct

open S
   
fun isRedundant {cases: 'a vector,
		 equals: 'a * 'a -> bool}: bool =
   let
      val nCases = Vector.length cases
   in
      0 < nCases
      andalso let
		 fun loop (i: int, prev: 'a): bool =
		    i < nCases
		    andalso let
			       val cur = Vector.sub (cases, i)
			    in
			       equals (cur, prev)
			       orelse loop (i + 1, cur)
			    end
	      in
		 loop (1, Vector.sub (cases, 0))
	      end
   end

fun exhaustiveAndIrredundant {all: 'a vector,
			      cases: 'a vector,
			      default: 'c option,
			      equals: 'a * 'a -> bool}: bool =
   Vector.isSubsequence (cases, all, equals)
   andalso (if Vector.length all = Vector.length cases
	       then Option.isNone default
	    else Option.isSome default)
   andalso not (isRedundant {cases = cases, equals = equals})

datatype t =
   EnumPointers of {enum: Label.t,
		     pointers: Label.t,
		     test: Use.t}
  | Int of {cases: (IntX.t * Label.t) vector,
	    default: Label.t option,
	    size: IntSize.t,
	    test: Use.t}
  | Pointer of {cases: {dst: Label.t,
			tag: int,
			tycon: PointerTycon.t} vector,
		default: Label.t option,
		tag: Use.t,
		test: Use.t} (* of type int*)
  | Word of {cases: (WordX.t * Label.t) vector,
	     default: Label.t option,
	     size: WordSize.t,
	     test: Use.t}

fun layout s =
   let
      open Layout
      fun simple ({cases, default, size = _, test}, name, lay) =
	 seq [str (concat ["switch", name, " "]),
	      record [("test", Use.layout test),
		      ("default", Option.layout Label.layout default),
		      ("cases",
		       Vector.layout
		       (Layout.tuple2 (lay, Label.layout))
		       cases)]]
   in
      case s of
	 EnumPointers {enum, pointers, test} =>
	    seq [str "SwitchEP ",
		 record [("test", Use.layout test),
			 ("enum", Label.layout enum),
			 ("pointers", Label.layout pointers)]]
       | Int z => simple (z, "Int", IntX.layout)
       | Pointer {cases, default, tag, test} =>
	    seq [str "SwitchPointer ",
		 record [("test", Use.layout test),
			 ("tag", Use.layout tag),
			 ("default", Option.layout Label.layout default),
			 ("cases",
			  Vector.layout
			  (fn {dst, tag, tycon} =>
			   record [("dst", Label.layout dst),
				   ("tag", Int.layout tag),
				   ("tycon", PointerTycon.layout tycon)])
			  cases)]]
       | Word z => simple (z, "Word", WordX.layout)
   end

fun isOk (s, {checkUse, labelIsOk}): bool =
   case s of
      EnumPointers {enum, pointers, test, ...} =>
	 (checkUse test
	  ; (labelIsOk enum
	     andalso labelIsOk pointers
	     andalso (case Use.ty test of
			 Type.EnumPointers _ => true
		       | _ => false)))
    | Int {cases, default, size, test} =>
	 (checkUse test
	  ; ((case default of
		 NONE => true
	       | SOME l => labelIsOk l)
	     andalso Vector.forall (cases, labelIsOk o #2)
	     andalso Vector.isSorted (cases, fn ((i, _), (i', _)) =>
				      IntX.<= (i, i'))
	     andalso
	     (case Use.ty test of
		 Type.EnumPointers {enum, pointers} =>
		    0 = Vector.length pointers
		    andalso
		    exhaustiveAndIrredundant
		    {all = Vector.map (enum, fn i =>
				       IntX.make (IntInf.fromInt i, size)),
		     cases = Vector.map (cases, #1),
		     default = default,
		     equals = IntX.equals}
	       | Type.Int s =>
		    IntSize.equals (size, s)
		    andalso Option.isSome default
		    andalso not (isRedundant
				 {cases = cases,
				  equals = fn ((i, _), (i', _)) =>
				  IntX.equals (i, i')})

	       | _ => false)))
    | Pointer {cases, default, tag, test} =>
	  (checkUse tag
	   ; checkUse test
	   ; (Type.equals (Use.ty tag, Type.defaultInt)
	      andalso (case default of
			  NONE => true
			| SOME l => labelIsOk l)
	      andalso Vector.forall (cases, labelIsOk o #dst)
	      andalso (Vector.isSorted
		       (cases,
			fn ({tycon = t, ...}, {tycon = t', ...}) =>
			PointerTycon.index t <= PointerTycon.index t'))
	      andalso
	      case Use.ty test of
		 Type.EnumPointers {enum, pointers} =>
		    0 = Vector.length enum
		    andalso 
		    exhaustiveAndIrredundant {all = pointers,
					      cases = Vector.map (cases, #tycon),
					      default = default,
					      equals = PointerTycon.equals}
	       | _ => false))
    | Word {cases, default, size, test} =>
	 (checkUse test
	  ; (Type.equals (Use.ty test, Type.word size)
	     andalso (case default of
			 NONE => false
		       | SOME l => labelIsOk l)
	     andalso Vector.forall (cases, labelIsOk o #2)
	     andalso Vector.isSorted (cases, fn ((w, _), (w', _)) =>
				      WordX.<= (w, w'))
	     andalso
	     not (isRedundant
		  {cases = cases,
		   equals = fn ((w, _), (w', _)) => WordX.equals (w, w')})))

fun foldLabelUse (s: t, a: 'a, {label, use}): 'a =
   let
      fun simple {cases, default, size = _, test} =
	 let
	    val a = use (test, a)
	    val a = Option.fold (default, a, label)
	    val a = Vector.fold (cases, a, fn ((_, l), a) =>
				 label (l, a))
	 in
	    a
	 end
   in
      case s of
	 EnumPointers {enum, pointers, test} =>
	  let
	     val a = use (test, a)
	     val a = label (enum, a)
	     val a = label (pointers, a)
	  in
	     a
	  end
	| Int z => simple z
	| Pointer {cases, default, tag, test} =>
	     let
		val a = use (tag, a)
		val a = use (test, a)
		val a = Option.fold (default, a, label)
		val a = Vector.fold (cases, a, fn ({dst, ...}, a) =>
				     label (dst, a))
	     in
		a
	     end
	| Word z => simple z
   end

fun foreachLabel (s, f) =
   foldLabelUse (s, (), {label = f o #1,
			 use = fn _ => ()})

end
