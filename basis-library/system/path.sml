(* Modified from the ML Kit 4.1.4; basislib/Path.sml
 * by mfluet@acm.org on 2005-8-10 based on
 *  modifications from the ML Kit 3 Version; basislib/Path.sml
 *  by sweeks@research.nj.nec.com on 1999-1-5.
 *)

structure OS_Path: OS_PATH =
struct

exception Path
exception InvalidArc

(* It would make sense to use substrings for internal versions of
 * fromString and toString, and to allocate new strings only when
 * externalizing the strings.

 * Impossible cases:
 UNIX: {isAbs = false, vol = _, arcs = "" :: _}
 Mac:  {isAbs = true,  vol = _, arcs = "" :: _}
 *)

val op @ = List.@
infix 9 sub
val op sub = String.sub
val substring = String.extract

(* Testing commands in both cygwin and mingw reveal that BOTH treat
 * paths exactly the same, and they also match newer windows console
 * commands (for example command.com and cmd.exe).
 * 
 * There is one exception: both cygwin and mingw treat /foo\bar
 * differently from \foo\bar; there is a special root for '/'.
 * This is so that cygwin and msys can fake a Unix directory tree.
 * 
 * Normal windows commands do not do this. Both msys and cygwin do it
 * differently. The msys(mingw) approach is for the shell(bash) to
 * translate the path before calling the command, eg: foo /usr will
 * run foo with arguement "c:/msys/1.0/". Under cygwin, the path is
 * passed through as stated and the program has to deal with it. Thus,
 * for mingw we can (and should) ignore the issue and thus the mlton
 * compiled application is identical to a windows app. However, under
 * cygwin, we need to track /* as a special volume.
 *)
val isWindows = 
   let 
      open Primitive.MLton.Platform.OS 
   in 
      host = MinGW orelse host = Cygwin
   end

val volumeHack =
   let
      open Primitive.MLton.Platform.OS
   in
      host = Cygwin
   end

(* the path separator used in canonical paths *)
val slash = if isWindows then "\\" else "/"

(* newer windows commands treat both / and \ as path separators
 * try echo sdfsdf > foo/bar under windows command.com -- it works
 * 
 * Sadly this means that toString o fromString is not the identity
 * b/c foo/bar -> foo\bar. However, there's nothing else one can do!
 * This diverges from the standard.
 *)
fun isslash c = c = #"/" orelse (isWindows andalso c = #"\\")
fun iscolon c = c = #":"

(* Under cygwin, the special volume "/" denotes the cygwin pseudo-root
 *)
fun isVolumeName v =
   (isWindows andalso size v = 2 andalso 
    Char.isAlpha (v sub 0) andalso iscolon (v sub 1))
   orelse
   (volumeHack andalso v = "/")

fun volumeMatch (root, relative) =
   relative = ""
   orelse (isVolumeName root
           andalso isVolumeName relative
           andalso (Char.toUpper (root sub 0)
                    = Char.toUpper (relative sub 0)))

fun canonName a = 
   if isWindows
      then String.translate (str o Char.toLower) a
   else a

val parentArc  = ".."
val currentArc = "."

(* Ahh joy. The SML basis library standard and Windows paths. 
 * 
 * The big problem with windows paths is "\foo""
 * - It's not absolute, since chdir("A:\") may switch from "C:", thus
   *   changing the meaning of "\foo".
   *)
fun validVolume {isAbs, vol} = 
   if isWindows
      then isVolumeName vol orelse (not isAbs andalso vol = "")
   else vol = ""

fun fromString s =
   let
      val (vol, rest) = (* 4:foo has a volume of "4:" even tho invalid *)
         if isWindows andalso size s >= 2 andalso iscolon (s sub 1)
            then (substring (s, 0, SOME 2), substring (s, 2, NONE))
         else 
            if volumeHack andalso size s >= 1 andalso s sub 0 = #"/"
               then ("/", s)
            else ("", s)
      val (isAbs, arcs) =
         case (String.fields isslash rest) of
            "" :: [] => (false, [])
          | "" :: r => (true, r)
          | r => (false, r)
   in
      {arcs = arcs, isAbs = isAbs, vol = vol}
   end

val getVolume = #vol o fromString
val isAbsolute = #isAbs o fromString
val isRelative = not o isAbsolute

fun isArc s =
   s = ""
   orelse (case fromString s of
              {arcs = [_], isAbs = false, vol = ""} => true
            | _ => false)

fun toString {arcs, isAbs, vol} =
   if not (validVolume {isAbs = isAbs, vol = vol})
      then raise Path
   else if not isAbs andalso case arcs of ("" :: _) => true | _ => false
      then raise Path
   else if List.exists (not o isArc) arcs
      then raise InvalidArc
   else 
      concat [vol,
              if isAbs andalso (not volumeHack orelse vol <> "/")
                 then slash
              else "",
              String.concatWith slash arcs]

fun concatArcs (a1, a2) =
   let
      val a1 = case List.rev a1 of "" :: r => List.rev r | _ => a1
   in
      a1 @ a2
   end

fun concat (p1, p2) =
   let
      val {arcs = a1, isAbs, vol = v1} = fromString p1
      val {arcs = a2, isAbs = isAbs2, vol = v2} = fromString p2
   in
      if isAbs2 orelse not (volumeMatch (v1, v2))
         then raise Path
      else toString {arcs = concatArcs (a1, a2), isAbs = isAbs, vol = v1}
   end

fun getParent p =
   let
      val {isAbs, vol, arcs} = fromString p
      val arcs =
         List.rev (case List.rev arcs of
                      [] => [parentArc]
                    | "." :: r => parentArc :: r
                    | ".." :: r => parentArc :: parentArc :: r
                    | _ :: [] => if isAbs then [""] else [currentArc]
                    | "" :: r => parentArc :: r
                    | _ :: r => r)
   in
      toString {arcs = arcs, isAbs = isAbs, vol = vol}
   end

fun mkCanonical p =
   let
      val {arcs, isAbs, vol} = fromString p
      fun backup l =
         case l of
            [] => if isAbs then [] else [parentArc]
          | first :: res =>
               if first = ".."
                  then parentArc :: parentArc :: res
               else res
      fun reduce arcs =
         let
            fun h (l, res) =
               case l of
                  [] => (case res of
                            [] => if isAbs then [""] else [currentArc]
                          | _ => res )
                | a1 :: ar =>
                     if a1 = "" orelse a1 = "."
                        then h (ar, res)
                     else if a1 = ".."
                             then h (ar, backup res)
                          else h (ar, canonName a1 :: res)
         in
            h (arcs, [])
         end
   in
      toString {arcs = List.rev (reduce arcs),
                isAbs = isAbs,
                vol = canonName vol}
   end

val rec parentize =
   fn [] => []
    | _ :: ar => parentArc :: parentize ar

fun mkRelative {path = p1, relativeTo = p2} =
   let
      val {arcs = arcs1, isAbs = isAbs1, vol = vol1} = fromString p1
      val {arcs = arcs2, isAbs = isAbs2, vol = vol2} =
         fromString (mkCanonical p2)
   in
      if not isAbs2 then raise Path
      else if not isAbs1 then p1
           else
              let
                 fun h (a1, a2) =
                    case (a1, a2) of
                       ([], []) => ["."]
                     | (_, []) => a1
                     | ([], a2) => parentize a2
                     | (a11 :: a1r, a21 :: a2r) =>
                          if canonName a11 = a21 then h (a1r, a2r)
                          else parentize a2 @ (if arcs1 = [""] then [] else a1)
              in
                 if not (volumeMatch (vol2, vol1))
                    then raise Path
                 else toString {arcs = h (arcs1, arcs2),
                                isAbs = false,
                                vol = ""}
              end
   end

fun mkAbsolute {path = p1, relativeTo = p2} =
   if isRelative p2 then raise Path
   else if isAbsolute p1 then p1
   else mkCanonical (concat (p2, p1))

fun isCanonical p = mkCanonical p = p

fun joinDirFile {dir, file} =
   let
      val {arcs, isAbs, vol} = fromString dir
      val arcs = 
         case (arcs, file) of
            ([], "") => []
          | _ => concatArcs (arcs, [file])
   in
      toString {arcs = arcs,
                isAbs = isAbs,
                vol = vol}
   end

fun splitDirFile p =
   let
      open List
      val {isAbs, vol, arcs} = fromString p
   in
      case rev arcs of
         [] => {dir = p, file = ""}
       | arcn :: farcs =>
            {dir = toString {arcs = rev farcs, isAbs = isAbs, vol = vol},
             file = arcn}

   end

val dir = #dir o splitDirFile

val file = #file o splitDirFile

fun joinBaseExt {base, ext} =
   case ext of
      NONE => base
    | SOME ex =>
         if ex = "" then base
         else String.concat [base, ".", ex]

fun splitBaseExt s =
   let
      val {dir, file} = splitDirFile s
      open Substring
      val (fst, snd) = splitr (fn c => c <> #".") (full file)
   in
      if isEmpty snd         (* dot at right end     *)
         orelse isEmpty fst  (* no dot               *)
         orelse size fst = 1 (* dot at left end only *)
         then {base = s, ext = NONE}
      else {base = joinDirFile {dir = dir,
                                file = string (trimr 1 fst)},
            ext = SOME (string snd)}
   end

val ext = #ext o splitBaseExt
val base = #base o splitBaseExt

fun isRoot path =
   case fromString path of
      {isAbs = true,  arcs=[""],  ...} => true
    | _ => false

fun fromUnixPath s = 
   if not isWindows then s
   else if Char.contains s (slash sub 0) then raise InvalidArc
   else String.translate (fn c => if c = #"/" then slash else str c) s

fun toUnixPath s = 
   if not isWindows then s
   else
      let
         val {arcs, isAbs, vol} = fromString s
      in
         if vol <> "" andalso not (volumeHack andalso vol = "/") 
            then raise Path 
         else (if isAbs then "/" else "") ^ String.concatWith "/" arcs
      end

end
