(* Modified from MLKitV3 basislib/Path.sml
 * by sweeks@research.nj.nec.com on 1999-1-5.
 *)

(*Path.sml*)

structure OS_Path : OS_PATH = struct
  exception Path
  exception InvalidArc

  (* It would make sense to use substrings for internal versions of
   * fromString and toString, and to allocate new strings only when
   * externalizing the strings.

   * Impossible cases:
     UNIX: {isAbs = false, vol = _, arcs = "" :: _}
     Mac:  {isAbs = true,  vol = _, arcs = "" :: _}
  *)

  local
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

      (* the path seperator used in canonical paths *)
      val slash = if isWindows then "\\" else "/"
      
      (* newer windows commands treat both / and \ as path seperators
       * try echo sdfsdf > foo/bar under windows command.com -- it works
       * 
       * Sadly this means that toString o fromString is not the identity
       * b/c foo/bar -> foo\bar. However, there's nothing else one can do!
       * This diverges from the standard.
       *)
      fun isslash c = c = #"/"    orelse (isWindows andalso c = #"\\")
      fun iscolon c = c = #":"
      
      (* characters disallowed in paths *)
      fun isbad   c = c = #"\000" orelse (isWindows andalso iscolon c)
      
      (* Under cygwin, the special volume "/" denotes the cygwin pseudo-root
       *)
      fun isVolumeName v =
         (isWindows andalso size v = 2 andalso 
          Char.isAlpha (v sub 0) andalso iscolon (v sub 1))
         orelse
         (volumeHack andalso v = "/")
      
      fun volumeMatch (root, relative) =
         relative = "" orelse
          (isVolumeName root) andalso (isVolumeName relative) andalso
           Char.toUpper (root sub 0) = Char.toUpper (relative sub 0)
  in
  
  val parentArc  = ".."
  val currentArc = "."
  
  (* Ahh joy. The SML basis library standard and Windows paths. 
   * 
   * The big problem with windows paths is "\foo""
   * - It's not absolute, since chdir("A:\") may switch from "C:", thus
   *   changing the meaning of "\foo".
   * - However, it's different from (and 'more absolute' than) "foo"
   *
   * Somehow, we need to distinguish "\foo" and "foo" without using isAbs
   * like is done for Unix paths. Trying to keep the leading "\" in the
   * arc leads to a mess of interactions later, so I don't do this.
   * It seems to make the most sense to just allow a leading "" for
   * non-absolute paths under windows. This has implications only in
   * the implementation of mkCanonical, concat, and isRoot.
   * 
   * I propose for Windows:
   * "E:foo"  => { isAbs=false, vol="E:", arcs=["foo"]  }
   * "E:\foo" => { isAbs=true,  vol="E:", arcs=["foo"]  }
   * "\foo"   => { isAbs=false, vol="",   arcs=["", "foo"] }
   * "foo"    => { isAbs=false, vol="",   arcs=["foo"]  }
   * "/foo"   => { isAbs=true,  vol="/",  arcs=["foo"]  } (cygwin volumeHack)
   *
   * For UNIX:
   * "foo"    => { isAbs=false, vol="",   arcs=["foo"]  }
   * "/foo"   => { isAbs=true,  vol="",   arcs=["foo"]  }
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
              if volumeHack andalso size s >= 1 andalso (s sub 0) = #"/"
              then ("/", s)
              else ("", s)
              
        val (isAbs, arcs) =
           case (String.fields isslash rest) of
                "" :: [] => (false, [])
              | "" :: r => 
                 if isWindows andalso vol = "" 
                 then (false, "" :: r)
                 else (true, r)
              | r => (false, r)
     in
        {isAbs=isAbs, vol=vol, arcs=arcs}
     end
      
  val getVolume  = #vol   o fromString
  val isAbsolute = #isAbs o fromString
  val isRelative = not o isAbsolute
  
  (* MLton previously rejected "foo/bar" as an arc.
   * Reading the standard shows that this is NOT a problem.
   * What is more of a problem would be having a null in a filename!
   * Under windows, a ":" may also not be in a filename.
   * 
   * See toString: "provided no exception is raised and none of the strings
   * in arcs contains an embedded arc separator character" -- this means
   * that containing an embedded arc separator character does NOT raise an
   * exception.
   *)
  fun isArc s = List.length (String.fields isbad s) = 1
  
  fun toString {arcs, isAbs, vol} =
     if not (validVolume {isAbs = isAbs, vol = vol})
	then raise Path
     else if not isWindows andalso not isAbs andalso 
             case arcs of ("" :: _) => true | _ => false
        then raise Path
     else if List.exists (not o isArc) arcs
	then raise InvalidArc
     else 
        vol ^ 
        (if isAbs andalso (not volumeHack orelse vol <> "/") then slash else "") ^ 
        String.concatWith slash arcs

  (* The standard doesn't address:
   *    concat("E:foo", "\foo") --> I say, raise Path
   *)
  fun concat (p1, p2) =
     let
        fun cutEmptyTail l = 
           List.rev (case List.rev l of ("" :: r) => r | l => l)
        fun concatArcs (a1, []) = a1
          | concatArcs (a1, a2) = cutEmptyTail a1 @ a2
        fun illegalJoin (_ :: _, "" :: _) = true
          | illegalJoin _ = false
     in
        case (fromString p1, fromString p2) of
             (_, {isAbs=true, ...}) => raise Path
           | ({isAbs, vol=v1, arcs=a1}, {vol=v2, arcs=a2, ...}) =>
              if not (volumeMatch (v1, v2))
                 then raise Path
              else if isWindows andalso illegalJoin (a1, a2)
                 then raise Path
              else toString { isAbs=isAbs, vol=v1, arcs=concatArcs (a1, a2) }
     end

  fun getParent p =
      let
	 val {isAbs, vol, arcs} = fromString p
	 val newarcs = List.rev (case List.rev arcs of
	      [] => [parentArc]
            | "." :: r => parentArc :: r
            | ".." :: r => parentArc :: parentArc :: r
            | _ :: [] => if isAbs then [""] else [currentArc]
            | _ :: "" :: [] => ["", ""] (* \ *)
            | "" :: r => parentArc :: r
	    | _ :: r => r)
      in
         toString {isAbs=isAbs, vol=vol, arcs=newarcs}
      end

  fun mkCanonical p =
      let val {isAbs, vol, arcs} = fromString p
        
          fun canonName a = 
             if isWindows
             then String.translate (Char.toString o Char.toLower) a
             else a
          
          val driveTop = case arcs of "" :: _ => true | _ => false
          val isRoot = isAbs orelse driveTop
          val bump = if driveTop andalso not isAbs then [""] else []
        
	  fun backup l =
	     case l of
		[] => if isRoot then [] else [parentArc]
	      | first :: res =>
		   if first = ".."
		      then parentArc :: parentArc :: res
		   else res
		   
	  fun reduce arcs =
	      let
		 fun h l res =
		    case l of
		       [] => (case res of
				 [] => if isRoot then bump @ [""] else [currentArc]
			       | _ => res @ bump)
		     | a1 :: ar =>
			  if a1 = "" orelse a1 = "."
			     then h ar res
			  else if a1 = ".."
			     then h ar (backup res)
		          else h ar (canonName a1 :: res)
	      in h arcs [] end
      in
	  toString {isAbs=isAbs, vol=canonName vol, arcs=List.rev (reduce arcs)}
      end

  fun parentize []      = []
    | parentize (_::ar) = parentArc :: parentize ar

  fun hackRoot {vol, arcs=""::r, ...} = {isAbs=true, vol=vol, arcs=r}
    | hackRoot x = x
  
  fun mkRelative {path = p1, relativeTo = p2} =
      case (hackRoot (fromString p1), hackRoot (fromString (mkCanonical p2))) of
	  (_ ,                {isAbs=false,...}) => raise Path
	| ({isAbs=false,...}, _                ) => p1
	| ({vol=vol1, arcs=arcs1,...}, {vol=vol2, arcs=arcs2, ...}) =>
	      let fun h [] [] = ["."]
		    | h a1 [] = a1
		    | h [] a2 = parentize a2
		    | h (a1 as (a11::a1r)) (a2 as (a21::a2r)) =
		      if a11=a21 then h a1r a2r
		      else parentize a2 @ (if arcs1 = [""] then [] else a1)
	      in
		  if not (volumeMatch (vol2, vol1)) then raise Path
		  else toString {isAbs=false, vol="", arcs=h arcs1 arcs2}
	      end

  fun mkAbsolute {path = p1, relativeTo = p2} =
      if isRelative p2 then raise Path
      else if isAbsolute p1 then p1
      else mkCanonical (concat (p2, p1))

  fun isCanonical p = mkCanonical p = p
	
  fun joinDirFile {dir, file} =
     if isArc file then concat (dir, file) else raise InvalidArc

  fun splitDirFile p =
      let open List
	  val {isAbs, vol, arcs} = fromString p
      in
	  case rev arcs of
	      []            => {dir = p, file = "" }
	    | arcn :: farcs =>
		  {dir = toString {isAbs=isAbs, vol=vol, arcs=rev farcs},
		   file = arcn}

      end

  fun dir s  = #dir  (splitDirFile s)
  fun file s = #file (splitDirFile s)

  fun joinBaseExt {base, ext} =
     case ext of
	NONE => base
      | SOME ex =>
	   if ex = ""
	      then base
	   else String.concat [base, ".", ex]

  fun splitBaseExt s =
      let val {dir, file} = splitDirFile s
	  open Substring
	  val (fst, snd) = splitr (fn c => c <> #".") (full file)
      in
	  if isEmpty snd         (* dot at right end     *)
	     orelse isEmpty fst  (* no dot               *)
	     orelse size fst = 1 (* dot at left end only *)
	      then {base = s, ext = NONE}
	  else
	      {base = joinDirFile{dir = dir,
				  file = string (trimr 1 fst)},
	       ext = SOME (string snd)}
      end

  fun ext s  = #ext  (splitBaseExt s)
  fun base s = #base (splitBaseExt s)

  fun isRoot path =
     case fromString path of
	{isAbs = true,  arcs=[""],  ...} => true
      | {isAbs = false, arcs=["", ""], ...} => isWindows
      | _ => false
  
  fun fromUnixPath s = 
     if not isWindows then s else
     if not (isArc s) then raise InvalidArc else
     String.translate (fn c => if c = #"/" then slash else Char.toString c) s
     
  fun toUnixPath s = 
     if not isWindows then s else
     let
        val {arcs, isAbs, vol} = fromString s
     in
        if vol <> "" andalso not (volumeHack andalso vol = "/") 
        then raise Path 
        else (if isAbs then "/" else "") ^ String.concatWith "/" arcs
     end
     
  end
end (*structure Path*)
