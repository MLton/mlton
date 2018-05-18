(* Copyright (C) 2004-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure List =
   struct
      open List

      fun sort f l =
         let
            fun qsort l =
               case l of
                  [] => []
                | hd::tl => 
                     let
                        val (lt,eq,gt) =
                           List.foldr
                           (fn (x,(lt,eq,gt)) =>
                            case f (x,hd) of
                               LESS => (x::lt,eq,gt)
                             | EQUAL => (lt,x::eq,gt)
                             | GREATER => (lt,eq,x::gt))
                           ([],[],[])
                           tl
                        val lt = qsort lt
                        val gt = qsort gt
                     in
                        lt @ (hd :: eq) @ gt
                     end
         in
            qsort l
         end
   end

structure Substring =
   struct
      open Substring

      fun droplSpace ss =
         dropl Char.isSpace ss
      fun droprSpace ss =
         dropr Char.isSpace ss

   end

structure Name =
   struct
      datatype t = T of string list

      fun compare (T ss1, T ss2) =
         List.collate 
         (fn (s1,s2) =>
          String.compare (CharVector.map Char.toLower s1,
                          CharVector.map Char.toLower s2))
         (ss1, ss2)

      fun last (T ss) = List.last ss
      fun toC (T ss) =
         String.concatWith "_" ss
      fun toML (T ss) =
         String.concatWith "." ss

      fun parse ss =
         let
            val ss = Substring.droplSpace ss
            val (names, rest) = 
               Substring.splitl 
               (fn c => Char.isAlphaNum c 
                   orelse c = #"." orelse c = #"_")
               ss
            val rest = Substring.droplSpace rest
         in
            if Substring.isEmpty names
               then NONE
               else let
                       val names = Substring.fields (fn c => #"." = c) names
                       val names = List.map Substring.string names
                    in
                       SOME (T names, rest)
                    end
         end
   end


structure Type =
   struct
      datatype t = 
         Array of t
       | Base of Name.t
       | Con of Name.t * t
       | Ref of t
       | Unit 
       | Vector of t

      fun toC t =
         case t of
            Array t => concat ["Array(", toC t, ")"]
          | Base name => Name.toC name
          | Con (name, t) => concat [Name.toC name, "(", toC t, ")"]
          | Ref t => concat ["Ref(", toC t, ")"]
          | Unit => "void"
          | Vector t => concat ["Vector(", toC t, ")"]
      fun toML t =
         case t of
            Array t => concat ["(", toML t, ") array"]
          | Base name => Name.toML name
          | Con (name, t) => concat ["(", toML t, ") ", Name.toML name]
          | Ref t => concat ["(", toML t, ") ref"]
          | Unit => "unit"
          | Vector t => concat ["(", toML t, ") vector"]

      fun parse s =
         let
            fun loop (s, t) =
               case Name.parse s of
                  NONE => (t, s)
                | SOME (Name.T ["array"], rest) => loop (rest, Array t)
                | SOME (Name.T ["ref"], rest) => loop (rest, Ref t)
                | SOME (Name.T ["vector"], rest) => loop (rest, Vector t)
                | SOME (name, rest) => loop (rest, Con (name, t))
         in
            case Name.parse s of
               NONE => raise Fail (concat ["Type.parse: \"", Substring.string s, "\""])
             | SOME (Name.T ["unit"], rest) => loop (rest, Unit)
             | SOME (name, rest) => loop (rest, Base name)
         end

      fun parseFn s =
         let
            fun loop (s, args) =
               let
                  val (arg, rest) = parse s
               in
                  if Substring.isPrefix "*" rest
                     then let
                             val rest = #2 (Substring.splitAt (rest, 1))
                          in
                             loop (rest, arg::args)
                          end
                  else if Substring.isPrefix "->" rest
                     then let
                             val rest = #2 (Substring.splitAt (rest, 2))
                             val (ret, rest) = parse rest
                          in
                             ({args = List.rev (arg::args),
                               ret = ret}, 
                              rest)
                          end
                  else raise Fail (concat ["Type.parseFn: \"", Substring.string s, "\""])
               end
         in
            loop (s, [])
         end
   end

structure Entry =
   struct
      datatype t =
         Const of {name: Name.t,
                   ty: Type.t}
       | Import of {attrs: string,
                    name: Name.t,
                    ty: {args: Type.t list,
                         ret: Type.t}}
       | Symbol of {name: Name.t,
                    ty: Type.t}

      fun name entry =
         case entry of
            Const {name,...} => name
          | Import {name,...} => name
          | Symbol {name,...} => name

      fun compare (entry1, entry2) =
         Name.compare (name entry1, name entry2)

      fun toC entry =
         case entry of
            Const {name, ty} => 
               String.concat
               ["PRIVATE extern const ",
                Type.toC ty,
                " ",
                Name.toC name,
                ";"]
          | Import {attrs, name, ty = {args, ret}} =>
               String.concat
               [attrs, 
                if String.size attrs > 0 then " " else "",
                Type.toC ret,
                " ",
                Name.toC name,
                "(",
                String.concatWith "," (List.map Type.toC args),
                ");"]
          | Symbol {name, ty} =>
               String.concat
               ["PRIVATE extern ",
                Type.toC ty,
                " ",
                Name.toC name,
                ";"]
      fun toML entry =
         case entry of
            Const {name, ty} =>
               String.concat
               ["val ",
                Name.last name,
                " = _const \"",
                Name.toC name,
                "\" : ",
                Type.toML ty,
                ";"]
          | Import {attrs, name, ty = {args, ret}} =>
               String.concat
               ["val ",
                Name.last name,
                " = _import \"",
                Name.toC name,
                "\" private : ",
                String.concatWith " * " (List.map Type.toML args),
                " -> ",
                Type.toML ret,
                ";"]
          | Symbol {name, ty} =>
               String.concat
               ["val (",
                Name.last name,
                "Get, ",
                Name.last name,
                "Set) = _symbol \"",
                Name.toC name,
                "\" private : (unit -> (",
                Type.toML ty,
                ")) * ((",
                Type.toML ty, 
                ") -> unit);"]

      fun parseConst (s, name) =
         let
            val s = #2 (Substring.splitAt (s, 6))
            val s = Substring.droplSpace s
            val s = if Substring.isPrefix ":" s
                      then #2 (Substring.splitAt (s, 1))
                       else raise Fail (concat ["Entry.parseConst: \"", Substring.string s, "\""])
            val (ret, rest) = Type.parse s
            val () = if Substring.isEmpty rest
                        then ()
                        else raise Fail (concat ["Entry.parseConst: \"", Substring.string s, "\""])
         in
            Const {name = name,
                   ty = ret}
         end

      fun parseImport (s, name) =
         let
            val s = #2 (Substring.splitAt (s, 7))
            val s = Substring.droplSpace s
            val (attrs, s) =
               case CharVectorSlice.findi (fn (_, c) => c = #":") s of
                  NONE => raise Fail (concat ["Entry.parseImport: \"", Substring.string s, "\""])
                | SOME (i, _) => Substring.splitAt (s, i)
            val attrs = Substring.droprSpace attrs
            val s = if Substring.isPrefix ":" s
                       then #2 (Substring.splitAt (s, 1))
                       else raise Fail (concat ["Entry.parseImport: \"", Substring.string s, "\""])
            val ({args, ret}, rest) = Type.parseFn s
            val () = if Substring.isEmpty rest
                        then ()
                        else raise Fail (concat ["Entry.parseImport: \"", Substring.string s, "\""])
         in
            Import {attrs = Substring.string attrs,
                    name = name,
                    ty = {args = args, ret = ret}}
         end

      fun parseSymbol (s, name) =
         let
            val s = #2 (Substring.splitAt (s, 7))
            val s = Substring.droplSpace s
            val s = if Substring.isPrefix ":" s
                      then #2 (Substring.splitAt (s, 1))
                       else raise Fail (concat ["Entry.parseSymbol: \"", Substring.string s, "\""])
            val (ret, rest) = Type.parse s
            val () = if Substring.isEmpty rest
                        then ()
                        else raise Fail (concat ["Entry.parseSymbol: \"", Substring.string s, "\""])
         in
            Symbol {name = name,
                    ty = ret}
         end

      fun parse s =
         case Name.parse s of
            NONE => raise Fail "Entry.parse"
          | SOME (name, rest) =>
               if Substring.isPrefix "=" rest
                  then let
                          val rest = #2 (Substring.splitAt (rest, 1))
                          val rest = Substring.droplSpace rest
                       in
                          if Substring.isPrefix "_const" rest
                             then parseConst (rest, name)
                          else if Substring.isPrefix "_import" rest
                             then parseImport (rest, name)
                          else if Substring.isPrefix "_symbol" rest
                             then parseSymbol (rest, name)
                          else raise Fail (concat ["Entry.parse: \"", Substring.string s, "\""])
                       end
                  else raise Fail (concat ["Entry.parse: \"", Substring.string s, "\""])
   end

val entries =
   let
      val f = TextIO.openIn "basis-ffi.def"
      fun loop entries =
         case TextIO.inputLine f of
            NONE => List.rev entries
          | SOME s => 
               if String.isPrefix "#" s
                  then loop entries
                  else let
                          val entry = Entry.parse (Substring.full s)
                       in
                          loop (entry :: entries)
                       end
      val entries = loop []
      val () = TextIO.closeIn f
      val entries = List.sort Entry.compare entries
   in
      entries
   end

fun outputC entries =
   let
      val f = TextIO.openOut "basis-ffi.h"
      fun print s = TextIO.output (f, s)
      fun println s = if s <> "" then (print s; print "\n") else ()

      val () = println "/* This file is automatically generated.  Do not edit. */\n\n"
      val () = println "#ifndef _MLTON_BASIS_FFI_H_\n"
      val () = println "#define _MLTON_BASIS_FFI_H_\n"
      val () = List.app (fn entry => println (Entry.toC entry)) entries
      val () = println "#endif /* _MLTON_BASIS_FFI_H_ */"
      val () = TextIO.closeOut f
   in
      ()
   end

fun outputML entries =
   let
      val f = TextIO.openOut "basis-ffi.sml"
      fun print s = TextIO.output (f, s)
      fun println s = if s <> "" then (print s; print "\n") else ()

      val primStrs = 
         (List.map (fn n => "Char" ^ n) ["8", "16", "32"]) @
         (List.map (fn n => "Int" ^ n) ["8", "16", "32", "64"]) @
         (List.map (fn n => "Real" ^ n) ["32", "64"]) @
         (List.map (fn n => "Word" ^ n) ["8", "16", "32", "64"])

      val () = println "(* This file is automatically generated.  Do not edit. *)\n"
      val () = println "local open Primitive in "
      val () = println "structure PrimitiveFFI ="
      val () = println "struct"
      val cur =
         List.foldl
         (fn (entry, cur) => 
          let
             val Name.T names = Entry.name entry
             val str = List.rev (List.tl (List.rev names))
             fun loop (cur, str) =
                case (cur, str) of
                   ([], []) => ()
                 | ([], str) =>
                      List.app (fn s => 
                                (println ("structure " ^ s ^ " = ")
                                 ; println "struct"
                                 ; if List.exists (fn s' => s = s') primStrs
                                      then println ("type t = " ^ s ^ ".t")
                                      else ()))
                               str
                 | (cur, []) => 
                      List.app (fn _ => println "end") cur
                 | (c::cur,s::str) =>
                      if c = s
                         then loop (cur, str)
                         else (println "end"
                               ; loop (cur, s::str))
          in
             loop (cur, str)
             ; println (Entry.toML entry)
             ; str
          end)
         []
         entries
      val () = List.app (fn _ => println "end") cur
      val () = println "end"
      val () = println "end"
      val () = TextIO.closeOut f
   in
      ()
   end

val () = outputC entries
val () = outputML entries
