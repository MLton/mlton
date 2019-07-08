(* Copyright (C) 2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure UniqueString:
   sig
      val unique: string -> string
   end =
   struct
      val set: {next: unit -> int,
                hash: word,
                original: string} HashSet.t =
         HashSet.new {hash = #hash}

      fun unique (s: string): string =
         let
            val hash = String.hash s
            val {next, ...} =
               HashSet.lookupOrInsert
               (set, hash, fn {original, ...} => s = original,
                fn () => {next = Counter.generator 0,
                          hash = hash,
                          original = s})
         in
            concat [s, "_", Int.toString (next ())]
         end
   end

functor Id (S: ID_STRUCTS): ID =
struct

open S

structure Plist = PropertyList

datatype t = T of {hash: word,
                   originalName: string,
                   printName: string option ref,
                   plist: Plist.t}
type id = t

local
   fun make f (T r) = f r
in
   val hash = make #hash
   val originalName = make #originalName
   val plist = make #plist
end

fun isAlphaNum (s: string): bool =
   String.forall (s, fn c => Char.isAlphaNum c orelse c = #"_")

fun clearPrintName (T {originalName, printName, ...}): unit =
   if isAlphaNum originalName
      then ()
   else printName := NONE

val printNameAlphaNumeric: bool ref = ref false

fun toString (T {originalName, printName, ...}) =
   case !printName of
      NONE =>
         let
            val s =
               if not (!printNameAlphaNumeric)
                  orelse isAlphaNum originalName
                  then originalName
               else
                  String.translate
                  (originalName,
                   fn #"!" => "Bang"
                    | #"#" => "Hash"
                    | #"$" => "Dollar"
                    | #"%" => "Percent"
                    | #"&" => "Ampersand"
                    | #"'" => "Prime"
                    | #"*" => "Star"
                    | #"+" => "Plus"
                    | #"-" => "Minus"
                    | #"." => "Dot"
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
                    | c => str c)
            val s = UniqueString.unique s
            val _ = printName := SOME s
         in
            s
         end
    | SOME s => s

val layout = String.layout o toString

fun equals (id, id') = Plist.equals (plist id, plist id')

local
   fun make (originalName, printName) =
      T {hash = Random.word (),
         originalName = originalName,
         printName = ref printName,
         plist = Plist.new ()}
in
   fun fromString s = make (s, SOME s)
   fun newString s = make (s, NONE)
end

local
   open Parse
   infix  1 <|> >>=
   infix  3 *>
   infixr 4 <$> <$$> <$$$>

   val cache =
      HashTable.new {hash = String.hash,
                     equals = String.equals}
   fun insert id =
      (ignore o HashTable.lookupOrInsert)
      (cache, toString id, fn () => id)

   val alphanum =
      named ("alphanum", nextSat (fn c => Char.isAlphaNum c orelse c = #"_" orelse c = #"'"))
   val sym =
      named ("sym", nextSat (fn c => String.contains ("!%&$#+-/:<=>?@\\~`^|*", c)))

   val alphanumId =
      (op ::) <$$> (named ("alpha", nextSat Char.isAlpha), many alphanum)
   val symId =
      (fn (c,cs,suf) => (c::(cs@suf))) <$$$>
      (sym, many sym,
       (op ::) <$$> (char #"_", many (nextSat Char.isDigit))
       <|> pure [])
   val tyvarId =
      (op ::) <$$> (nextSat (fn c => c = #"'"), many alphanum)

   fun parseGen (alts: (string * 'a Parse.t) vector, fromId: id -> 'a Parse.t) : 'a Parse.t =
      mlSpaces *>
      (String.implode <$>
       (if String.sub (noname, 0) = #"'"
           then tyvarId
           else alphanumId <|> symId)) >>= (fn printName =>
      let
         fun make () =
           let
              fun loop (i, b) =
                if Char.isDigit (String.sub (printName, i))
                   then loop (i - 1, true)
                else if b andalso String.sub (printName, i) = #"_"
                        then newString (String.substring (printName, 0, i))
                        else fromString printName
           in
              loop (String.size printName - 1, false)
           end
      in
         case Vector.peek (alts, fn (s, _) => String.equals (printName, s)) of
            SOME (_, res) => res
          | NONE => fromId (HashTable.lookupOrInsert (cache, printName, make))
      end)
in
   fun parseAs (alts, fromId) = parseGen (Vector.map (alts, fn (s, r) => (s, pure r)), pure o fromId)
   fun parseExcept ss = parseGen (Vector.map (ss, fn s => (s, fail "fail")), pure)
   val parse = parseExcept (Vector.new0 ())
   fun parseReset {prims} =
      (HashTable.removeAll (cache, fn _ => true);
       Vector.foreach (prims, insert))
end

val new = newString o originalName

fun newNoname () = newString noname

val bogus = newString "bogus"

val clear = Plist.clear o plist

end
