(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Const (S: CONST_STRUCTS): CONST = 
struct

open S

local open Ast
in structure Aconst = Const
end

structure Type =
   struct
      type t = Tycon.t * Tycon.t vector
      fun equals ((tc1,tcs1), (tc2,tcs2)) =
	 Tycon.equals (tc1, tc2)
	 andalso
	 Vector.equals (tcs1, tcs2, Tycon.equals)
      fun toType ((tc,tcs), con) =
	 con (tc, Vector.map (tcs, fn tc => con (tc, Vector.new0())))
      val layout = Ast.Type.layout o (fn t => 
				      toType (t, fn (t, ts) => 
					      Ast.Type.con (Tycon.toAst t, ts)))
      val toString = Layout.toString o layout
      fun make (tc, tcs) : t = (tc, tcs)
      fun unary (tc, tc') = make (tc, Vector.new1 tc')
      fun nullary tc = make (tc, Vector.new0())
      val bool = nullary Tycon.bool
      val char = nullary Tycon.char
      val int = nullary Tycon.defaultInt
      val intInf = nullary Tycon.intInf
      val real = nullary Tycon.real
      val word = nullary Tycon.word
      val word8 = nullary Tycon.word8
      val string = unary (Tycon.vector, Tycon.char)
   end

structure Node =
   struct
      datatype t =
	 Char of char
       | Int of int
       | IntInf of IntInf.t
       | Real of string
       | String of string
       | Word of word

      local
	 open Layout
	 fun wrap (pre, post, s) = seq [str pre, String.layout s, str post]
      in
	 val layout =
	    fn Char c => wrap ("#\"", "\"", String.implode [c])
	     | Int n => str (Int.toString n)
	     | IntInf s => IntInf.layout s
	     | Real r => String.layout r
	     | String s => wrap ("\"", "\"", s)
	     | Word w => seq [str "0wx", str (Word.toString w)]
      end
   end

datatype z = datatype Node.t
datatype t = T of {node: Node.t,
		   ty: Type.t}

local
   fun make sel (T r) = sel r
in
   val node = make #node
   val ty = make #ty
end

val layout = Node.layout o node
val toString = Layout.toString o layout
   
fun make (n, t) = T {node = n, ty = t}

local
   val char = Random.word ()
   val truee = Random.word ()
   val falsee = Random.word ()
in
   fun hash (c: t): word =
      case node c of
	 Char c => Word.xorb (char, Word.fromChar c)
       | Int i => Word.fromInt i
       | IntInf i => String.hash (IntInf.toString i)
       | Real r => String.hash r
       | String s => String.hash s
       | Word w => w
end
   
fun 'a toAst (make: Ast.Const.t -> 'a, constrain: 'a * Ast.Type.t -> 'a) c =
   let
      val make = fn n => make (Ast.Const.makeRegion (n, Region.bogus))
      fun maybeConstrain (defaultTycon, aconst) =
	 let
	    val ty = ty c
	    val con : Tycon.t * Ast.Type.t vector -> Ast.Type.t =
	       fn (t, ts) => Ast.Type.con (Tycon.toAst t, ts)
	 in
	    if Type.equals (ty, Type.nullary defaultTycon)
	       then make aconst
	    else constrain (make aconst, Type.toType (ty, con))
	 end
      fun int s = maybeConstrain (Tycon.defaultInt, Aconst.Int s)
   in
      case node c of
	 Char c => make (Aconst.Char c)
       | Int n => int (Int.toString n)
       | IntInf i => int (IntInf.toString i)
       | Real r => make (Aconst.Real r)
       | String s => make (Aconst.String s)
       | Word w => maybeConstrain (Tycon.defaultWord, Aconst.Word w)
   end

val toAstExp = toAst (Ast.Exp.const, Ast.Exp.constraint)
val toAstPat = toAst (Ast.Pat.const, Ast.Pat.constraint)

fun equals (c, c') =
   Type.equals (ty c, ty c')
   andalso
   case (node c, node c') of
      (Char c, Char c') => c = c'
    | (Int n, Int n') => n = n'
    | (IntInf i, IntInf i') => IntInf.equals (i, i')
    | (Real r, Real r') => String.equals (r, r')
    | (String s, String s') => String.equals (s, s')
    | (Word w, Word w') => w = w'
    | _ => false

val equals = Trace.trace2 ("Const.equals", layout, layout, Bool.layout) equals

local
   fun make c t x = T {node = c x, ty = t}
in
   val fromChar = make Char Type.char
   val fromInt = make Int Type.int
   val fromIntInf = make IntInf Type.intInf
   val fromReal = make Real Type.real
   val fromString = make String Type.string
   val fromWord = make Word Type.word
   val fromWord8 = make (fn w => Word (Word.fromWord8 w)) Type.word8
end

structure SmallIntInf =
   struct
      (*
       * The IntInf.fromInt calls are just because SML/NJ doesn't
       * overload integer constants for IntInf.int's.
       * This code relies on the language that MLton is implemented in using at
       * least 31 bits for integers.
       *)
      val minSmall: IntInf.t = IntInf.fromInt ~0x40000000
      val maxSmall: IntInf.t = IntInf.fromInt 0x3FFFFFFF

      fun isSmall (i: IntInf.t): bool =
	 let open IntInf
	 in minSmall <= i andalso i <= maxSmall
	 end

      fun toWord (i: IntInf.t): word option =
	 if isSmall i
	    then SOME (Word.orb (0w1,
				 Word.<< (Word.fromInt (IntInf.toInt i),
					  0w1)))
	 else NONE

      fun fromWord (w: word): IntInf.t option =
	 if w < 0wx80000000
	    then SOME (IntInf.fromInt (Word.toIntX (Word.~>> (w, 0w1))))
	 else NONE
   end
  
end
