(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor Id (S: ID_STRUCTS): ID =
struct

open S

structure Plist = PropertyList

(* Can't use the same hash as Symbol.hash original name, because many later
 * passes would be really slow due to lots of ids (with the same noname symbol)
 * hashing to the same value.
 *)
datatype t = T of {hash: word,
		   originalName: Symbol.t,
		   printName: string option ref,
		   plist: Plist.t}

local
   fun make f (T r) = f r
in
   val hash = make #hash
   val originalName = make #originalName
   val plist = make #plist
   val printName= make #printName
end

fun clearPrintName x = printName x := NONE
   
fun setPrintName (x, s) = printName x := SOME s

val printNameAlphaNumeric: bool ref = ref false
   
fun toString (T {printName, originalName, ...}) =
   case !printName of
      NONE =>
	 let
	    val s =
	       if not (!printNameAlphaNumeric)
		  orelse String.forall (Symbol.toString originalName, fn c =>
					Char.isAlphaNum c orelse c = #"_")
		  then originalName
	       else
		  Symbol.fromString
		  (String.translate
		   (Symbol.toString originalName,
		    fn #"!" => "Bang"
		     | #"#" => "Hash"
		     | #"$" => "Dollar"
		     | #"%" => "Percent"
		     | #"&" => "Ampersand"
		     | #"'" => "P"
		     | #"*" => "Star"
		     | #"+" => "Plus"
		     | #"-" => "Minus"
		     | #"." => "D"
		     | #"/" => "Divide"
		     | #":" => "Colon"
		     | #"<" => "Lt"
		     | #"=" => "Eq"
		     | #">" => "Gt"
		     | #"?" => "Ques"
		     | #"@" => "At"
		     | #"\\" => "Slash"
		     | #"^" => "Caret"
		     | #"`" => "Quote"
		     | #"|" => "Pipe"
		     | #"~" => "Tilde"
		     | c => str c))
	    val s = Symbol.uniqueString s
	    val _ = printName := SOME s
	 in
	    s
	 end
    | SOME s => s

val layout = String.layout o toString
   
fun sameName (id, id') = Symbol.equals (originalName id, originalName id')

fun equals (id, id') = Plist.equals (plist id, plist id')

local
   fun make (originalName, printName) =
      T {hash = Random.word (),
	 originalName = originalName,
	 printName = ref printName,
	 plist = Plist.new ()}
in
   fun fromString s =
      make (Symbol.fromString s,
	    SOME s)
   fun newSymbol s = make (s, NONE)
end

val new = newSymbol o originalName

val newString = newSymbol o Symbol.fromString

local
   val noname = Symbol.fromString noname
in
   fun newNoname () = newSymbol noname
end

val bogus = newSymbol Symbol.bogus

val clear = Plist.clear o plist
   
end
