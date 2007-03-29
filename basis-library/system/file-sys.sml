(* os-filesys.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The Posix implementation of the generic file system interface.
 *
 *)

structure OS_FileSys =
   struct
      structure P_FSys = Posix.FileSys

      val sysWordToWord = Word.fromLargeWord o SysWord.toLargeWord

      type dirstream = P_FSys.dirstream

      val openDir   = P_FSys.opendir
      val readDir   = P_FSys.readdir
      val rewindDir = P_FSys.rewinddir
      val closeDir  = P_FSys.closedir

      val chDir  = P_FSys.chdir
      val getDir = P_FSys.getcwd
      local
         structure S = P_FSys.S
         val mode777 = S.flags[S.irwxu, S.irwxg, S.irwxo]
      in
         fun mkDir path = P_FSys.mkdir(path, mode777)
      end
      val rmDir  = P_FSys.rmdir
      val isDir  = P_FSys.ST.isDir o P_FSys.stat

      val isLink   = P_FSys.ST.isLink o P_FSys.lstat
      val readLink = P_FSys.readlink

      (* the maximum number of links allowed *)
      val maxLinks: int = 64

      structure P = OS_Path

      val isMinGW = let open Primitive.MLton.Platform.OS in host = MinGW end

      (* An implementation of fullPath which works on Unix and Windows (Cygwin and MinGW) *)
      fun fullPath p =
         let
            val oldCWD = getDir()
            fun mkPath (pathFromRoot, vol) =
               P.toString {arcs = List.rev pathFromRoot,
                           isAbs = true,
                           vol = vol}
            fun walkPath (n, pathFromRoot, arcs, vol) =
               if n = 0
                  then raise PosixError.SysErr ("too many links", NONE)
               else
                  case arcs of
                     [] => mkPath (pathFromRoot, vol)
                   | arc :: al =>
                        if arc = "" orelse arc = "."
                           then walkPath (n, pathFromRoot, al, vol)
                        else if arc = ".."
                                then
                                   case pathFromRoot of
                                      [] => walkPath (n, [], al, vol)
                                    | _ :: r =>
                                         (chDir ".."; walkPath (n, r, al, vol))
                        else
                           if isLink arc
                              then expandLink (n, pathFromRoot, arc, al, vol)
                           else
                              case al of
                                 [] => mkPath (arc :: pathFromRoot, vol)
                               | _ =>
                                    (chDir arc
                                     ; walkPath (n, arc :: pathFromRoot, al, vol))
            and expandLink (n, pathFromRoot, link, rest, vol) =
               let
                  val {isAbs, arcs, ...} = P.fromString (readLink link)
                  val arcs = List.@ (arcs, rest)
               in 
                  if isAbs
                     then gotoRoot (n-1, arcs, vol)
                  else walkPath (n-1, pathFromRoot, arcs, vol)
               end
            (* If the volume is not empty, chDir to it rather than to "/" *)
            and gotoRoot (n, arcs, vol) =
                (if vol <> "" 
                    then chDir (vol ^ (if isMinGW then "\\" else "/"))
                 else chDir "/"
                 ; walkPath (n, [], arcs, vol))
            fun computeFullPath (arcs, vol) =
               (gotoRoot (maxLinks, arcs, vol) before chDir oldCWD)
               handle ex => (chDir oldCWD; raise ex)
         in
            case (P.fromString p)
               of {isAbs=false, arcs, ...} =>
                  let
                     val {arcs=arcs', vol=vol, ...} = P.fromString(oldCWD)
                  in
                     computeFullPath (List.@(arcs', arcs), vol)
                  end
             | {isAbs=true, arcs, vol} => computeFullPath (arcs, vol)
         end

      fun realPath p =
         if P.isAbsolute p
            then fullPath p
         else P.mkRelative {path = fullPath p,
                            relativeTo = fullPath (getDir ())}

      val fileSize = P_FSys.ST.size o P_FSys.stat

      val modTime = P_FSys.ST.mtime o P_FSys.stat

      fun setTime (path, t) =
         P_FSys.utime (path, Option.map (fn t => {actime = t, modtime = t}) t)

      val remove = P_FSys.unlink

      val rename = P_FSys.rename

      datatype access_mode = datatype Posix.FileSys.access_mode

      fun access (path, al) =
         let
            fun cvt A_READ = P_FSys.A_READ
              | cvt A_WRITE = P_FSys.A_WRITE
              | cvt A_EXEC = P_FSys.A_EXEC
         in
            P_FSys.access (path, List.map cvt al)
         end

      datatype file_id = FID of {dev: SysWord.word, ino: SysWord.word}

      fun fileId fname = let
                            val st = P_FSys.stat fname
                         in
                            FID{
                                dev = P_FSys.devToWord(P_FSys.ST.dev st),
                                ino = P_FSys.inoToWord(P_FSys.ST.ino st)
                                }
                         end

      fun hash (FID{dev, ino}) = sysWordToWord(SysWord.+(SysWord.<<(dev, 0w16), ino))

      fun compare (FID{dev=d1, ino=i1}, FID{dev=d2, ino=i2}) =
         if (SysWord.<(d1, d2))
            then General.LESS
         else if (SysWord.>(d1, d2))
                 then General.GREATER
              else if (SysWord.<(i1, i2))
                      then General.LESS
                   else if (SysWord.>(i1, i2))
                           then General.GREATER
                        else General.EQUAL

   end

(*
 * $Log: os-filesys.sml, v $
 * Revision 1.3  1997/06/07 15:27:51  jhr
 *   SML'97 Basis Library changes (phase 3; Posix changes)
 *
 * Revision 1.2  1997/02/26  21:00:32  george
 *    Defined a new top level Option structure. All 'a option related
 *    functions have been moved out of General.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
