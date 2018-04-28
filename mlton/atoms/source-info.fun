(* Copyright (C) 2003-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor SourceInfo (S: SOURCE_INFO_STRUCTS): SOURCE_INFO =
struct

open S

structure Pos =
   struct
      datatype t =
         Known of SourcePos.t
       | Unknown

      fun toString p =
         case p of
            Known p =>
               if !Control.profile = Control.ProfileCallStack
                  then SourcePos.toString p
               else concat [SourcePos.file p, ": ",
                            Int.toString (SourcePos.line p)]
          | Unknown => "<unknown>"

      fun fromRegion r =
         case Region.left r of
            NONE => Unknown
          | SOME p => Known p

      fun file p =
         case p of
            Known p => SOME (SourcePos.file p)
          | Unknown => NONE
   end

datatype info =
   Anonymous of Pos.t
 | C of string
 | Function of {name: string list,
                pos: Pos.t}

datatype t = T of {hash: word,
                   info: info,
                   plist: PropertyList.t}

local
   val r: t list ref = ref []
in
   fun new info =
      let
         val res = T {hash = Random.word (),
                      info = info,
                      plist = PropertyList.new ()}
         val () =
            if !Control.profile = Control.ProfileCount
               then List.push (r, res)
            else ()
      in
         res
      end
   fun all () = !r
end

local
   fun make f (T r) = f r
in
   val hash = make #hash
   val info = make #info
   val plist = make #plist
end

local
   val set: {hash: word,
             name: string,
             sourceInfo: t} HashSet.t =
      HashSet.new {hash = #hash}
in   
   fun fromC (name: string) =
      let
         val hash = String.hash name
      in
         #sourceInfo
         (HashSet.lookupOrInsert
          (set, hash, fn {hash = h, ...} => hash = h,
           fn () => {hash = hash,
                     name = name,
                     sourceInfo = new (C name)}))
      end
end

fun function {name, region} =
   new (Function {name = name,
                  pos = Pos.fromRegion region})

fun toString' (si, sep) =
   case info si of
      Anonymous pos => Pos.toString pos
    | C s => concat ["<", s, ">"]
    | Function {name, pos} =>
         concat [concat (List.separate (List.rev name, ".")),
                 sep, Pos.toString pos]

fun toString si = toString' (si, " ")

val layout = Layout.str o toString

val equals: t * t -> bool =
   fn (s, s') => PropertyList.equals (plist s, plist s')

val equals =
   Trace.trace2 ("SourceInfo.equals", layout, layout, Bool.layout) equals

fun file (s: t): File.t option =
   case info s of
      Anonymous pos => Pos.file pos
    | C _ => NONE
    | Function {pos, ...} => Pos.file pos

fun isC (s: t): bool =
   case info s of
      C _ => true
    | _ => false

val gc = fromC "gc"
val gcArrayAllocate = fromC "GC_arrayAllocate"
val main = fromC "main"
val polyEqual = fromC "poly-equal"
val polyHash = fromC "poly-hash"
val unknown = fromC "unknown"

end
