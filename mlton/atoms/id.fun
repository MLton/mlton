(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure IdCounter:
   sig
      val getCounter: string -> Counter.t
   end =
   struct
      val getCounter = String.memoize (fn _ => Counter.new 0)
   end

functor Id (S: ID_STRUCTS): ID =
struct

open S

structure Plist = PropertyList
   
datatype t = T of {originalName: string,
		   printName: string option ref,
		   plist: Plist.t}

local
   fun make f (T r) = f r
in
   val plist = make #plist
   val originalName = make #originalName
end

fun toString (T {printName, originalName, ...}) =
   case !printName of
      NONE =>
	 let
	    val s =
	       concat [String.translate (originalName,
					 fn #"'" => "P" | c => str c),
		       "_",
		       Int.toString (Counter.next
				     (IdCounter.getCounter originalName))]
	 in printName := SOME s
	    ; s
	 end
    | SOME s => s

val layout = String.layout o toString
   
fun sameName (id, id') = 
   String.equals (originalName id, originalName id')

fun equals (id, id') = Plist.equals (plist id, plist id')

fun fromString s =
   T {originalName = s,
     printName = ref (SOME s),
     plist = Plist.new ()}
   
fun isOperator s =
   let val c = String.sub (s, 0)
   in not (Char.isAlpha c orelse Char.equals (c, #"'"))
   end

fun translateOperator s =
   let val map
         = fn #"!" => "Bang"
            | #"%" => "Percent"
            | #"&" => "Ampersand"
            | #"$" => "Dollar"
            | #"#" => "Hash"
            | #"+" => "Plus"
            | #"-" => "Minus"
            | #"/" => "Divide"
            | #":" => "Colon"
            | #"<" => "Lt"
            | #"=" => "Eq"
            | #">" => "Gt"
            | #"?" => "Ques"
            | #"@" => "At"
            | #"\\" => "Slash"
            | #"~" => "Tilde"
            | #"`" => "Quote"
            | #"^" => "Caret"
            | #"|" => "Pipe"
            | #"*" => "Star"
            | _ => ""
   in
     "operator" ^ (String.translate(s, map))
   end

fun newString s =
   let val s = if isOperator s then translateOperator s else s
   in fn () => T {originalName = s,
		  printName = ref NONE,
		  plist = Plist.new ()}
   end

val newNoname = newString noname

val newString = fn s => newString s ()
   
val bogus = newString "BOGUS"

fun new (T {originalName, ...}) =
   T {originalName = originalName,
      printName = ref NONE,
      plist = Plist.new ()}

val fromAst = newString o AstId.toString
fun fromAsts l = List.map (l, fromAst)
val toAst = AstId.fromString o toString
fun toAsts l = List.map (l, toAst)

val clear = Plist.clear o plist
   
end

functor HashId (S: ID_STRUCTS): HASH_ID =
struct

open S

structure Plist = PropertyList
   
datatype t = T of {originalName: string,
		   printName: string option ref,
		   hash: Word.t,
		   plist: Plist.t}

local
   fun make f (T r) = f r
in
   val hash = make #hash
   val plist = make #plist
   val originalName = make #originalName
end

fun toString (T {printName, originalName, ...}) =
   case !printName of
      NONE =>
	 let
	    val s =
	       concat [String.translate (originalName,
					fn #"'" => "P" | c => str c),
		       "_",
		       Int.toString (Counter.next
				     (IdCounter.getCounter originalName))]
	 in printName := SOME s
	    ; s
	 end
    | SOME s => s

val layout = String.layout o toString
   
fun sameName (id, id') = 
   String.equals (originalName id, originalName id')

fun equals (id, id') = Plist.equals (plist id, plist id')

fun fromString s =
   T {originalName = s,
      printName = ref (SOME s),
      hash = Random.word (),
      plist = Plist.new ()}
   
fun isOperator s =
   let val c = String.sub (s, 0)
   in not (Char.isAlpha c orelse Char.equals (c, #"'"))
   end

fun translateOperator s =
   let val map
         = fn #"!" => "Bang"
            | #"%" => "Percent"
            | #"&" => "Ampersand"
            | #"$" => "Dollar"
            | #"#" => "Hash"
            | #"+" => "Plus"
            | #"-" => "Minus"
            | #"/" => "Divide"
            | #":" => "Colon"
            | #"<" => "Lt"
            | #"=" => "Eq"
            | #">" => "Gt"
            | #"?" => "Ques"
            | #"@" => "At"
            | #"\\" => "Slash"
            | #"~" => "Tilde"
            | #"`" => "Quote"
            | #"^" => "Caret"
            | #"|" => "Pipe"
            | #"*" => "Star"
            | _ => ""
   in
     "operator" ^ (String.translate(s, map))
   end

fun newString s =
   let val s = if isOperator s then translateOperator s else s
   in fn () => T {originalName = s,
		  printName = ref NONE,
		  hash = Random.word (),
		  plist = Plist.new ()}
   end

val newNoname = newString noname

val newString = fn s => newString s ()
   
val bogus = newString "BOGUS"

fun new (T {originalName, ...}) =
   T {originalName = originalName,
      printName = ref NONE,
      hash = Random.word (),
      plist = Plist.new ()}

val fromAst = newString o AstId.toString
fun fromAsts l = List.map (l, fromAst)
val toAst = AstId.fromString o toString
fun toAsts l = List.map (l, toAst)

val clear = Plist.clear o plist
   
end
