(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Const (S: CONST_STRUCTS): CONST = 
struct

open S

local open Ast
in structure Aconst = Const
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
		   tycon: Tycon.t}

local
   fun make sel (T r) = sel r
in
   val node = make #node
   val tycon = make #tycon
end

val layout = Node.layout o node
   
fun make (n, t) = T {node = n, tycon = t}

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
      fun maybeConstrain (defaultTycon, aconst) =
	 let val t = tycon c
	 in if Tycon.equals (t, defaultTycon)
	       then make aconst
	    else constrain (make aconst, Ast.Type.con (Tycon.toAst t,
						       Vector.new0 ()))
	 end
      fun int s = maybeConstrain (Tycon.defaultInt, Aconst.Int s)
   in case node c of
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
   Tycon.equals (tycon c, tycon c')
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

fun fromChar c = T {node = Char c, tycon = Tycon.char}
   
fun fromInt n = T {node = Int n, tycon = Tycon.defaultInt}

fun fromIntInf i = T {node = IntInf i, tycon = Tycon.intInf}

fun fromString s = T {node = String s, tycon = Tycon.string}

fun fromReal s = T {node = Real s, tycon = Tycon.real}

fun fromWord w = T {node = Word w, tycon = Tycon.word}
   
fun fromWord8 w = T {node = Word (Word.fromWord8 w), tycon = Tycon.word8}

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

      fun toWord (i: IntInf.t): word =
	 Word.orb (0w1, Word.<< (Word.fromInt (IntInf.toInt i), 0w1))

      fun fromWord (w: word): IntInf.t =
	 IntInf.fromInt (Word.toIntX (Word.~>> (w, 0w1)))
   end
  
end
