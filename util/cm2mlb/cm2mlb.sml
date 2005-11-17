(*
 * Author: Matthew Fluet (mfluet@acm.org) 
 *
 * This requires that you have SML/NJ installed.
 * It works with SML/NJ 110.47 and may require changes to work with other
 * versions, since it depends on the CM structure.
 *
 * cm2mlb takes a ".cm" file and prints on stdout a corresponding ".mlb".
 *
 * To use from the REPL, do the following:
 * CM2MLB.cm2mlb {defines = ["MLton"],
 *                maps = [],
 *                sources = "sources.cm",
 *                out = TextIO.stdOut}
 *
 * Before using from the shell, you must do the following, where <smlnj> is
 * the root directory of the SML/NJ installation.  You may need to be root in
 * order to do these.
 * 1. From the SML/NJ REPL:
 *      CM.make "sources.cm";
 *      CM2MLB.export ();
 * 2. ln -s <smlnj>/bin/.run-sml <smlnj>/bin/cm2mlb
 * 3. mv cm2mlb.x86-linux <smlnj>/bin/.heap
 *
 * Once it is installed, the usage is as follows:
 *   cm2mlb [-Dsym ...] [-map file] sources.cm
 *
 * -Dsym can be used to define CM preprocessor symbols.
 * -map file can be used to add cm2mlb mappings.
 *)

structure CM2MLB :
sig
   val cm2mlb : {defines: string list,
                 maps: string list,
                 out: TextIO.outstream,
                 sources: string} -> unit
   val main: string * string list -> OS.Process.status
   val export : unit -> unit
end =
struct
   structure PG = PortableGraph

   fun message s = TextIO.output (TextIO.stdErr, s ^ "\n")
   fun die msg =
      (message ("Error: " ^ msg)
       ; OS.Process.exit OS.Process.failure)

   structure CM =
      struct
         open CM

         structure Graph =
            struct
               val graph = fn src =>
                  (Graph.graph src)
                  handle _ => NONE
            end
      end

   structure AnchorMap =
      struct

         fun make (file : string) =
            if OS.FileSys.access (file, [OS.FileSys.A_READ])
               then 
                  let
                     val lines =
                        let
                           val f = TextIO.openIn file
                        in
                           let
                              fun loop lines =
                                 case TextIO.inputLine f of
                                    NONE => List.rev lines
                                  | SOME l => loop (l::lines)
                           in
                              loop []
                              before TextIO.closeIn f
                           end handle e => (TextIO.closeIn f; raise e)
                        end handle _ => []
                  in
                     List.mapPartial
                     (fn line =>
                      if CharVector.all Char.isSpace line
                         then NONE
                         else 
                            case String.tokens Char.isSpace line of
                               [cmAnchor, mlbPath] => 
                                  SOME {cmAnchor = cmAnchor, mlbPath = mlbPath}
                             | _ =>  die (concat ["strange cm->mlb mapping: ", 
                                                  file, ":: ", line]))
                     lines
                  end
               else []

         val default = make "cm2mlb-map"
      end
   
   fun cm2mlb {defines, maps, out, sources} =
      let
         (* Define preprocessor symbols *)
         val _ = 
            List.app 
            (fn sym => (#set (CM.symval sym)) (SOME 1))
            defines
         val _ = (#set CM.Control.verbose) false
         val _ = (#set CM.Control.warn_obsolete) false
         val _ = Control.printWarnings := false

         val _ =
            if OS.FileSys.access (sources, [OS.FileSys.A_READ])
               then ()
               else die (concat ["file not found: ", sources])
         val {dir, file = sources} = OS.Path.splitDirFile sources
         val () = if dir <> "" then OS.FileSys.chDir dir else ()

         local
            val anchorMap =
               List.concat
               ((List.map AnchorMap.make maps) @
                [AnchorMap.default])
               
            fun peekAnchorMap cmAnchor' =
               case List.find (fn {cmAnchor, ...} => cmAnchor = cmAnchor') anchorMap of
                  NONE => NONE
                | SOME {mlbPath, ...} => SOME mlbPath
         in
            val peekAnchorMap = peekAnchorMap
         end
      in
         case CM.Graph.graph sources of
            SOME {graph as PG.GRAPH {imports, ...}, imports = importLibs, nativesrc} =>
               let
                  val imports =
                     ListPair.map
                     (fn (bid, cmLib) =>
                      let
                         val cmLibDescr = CM.Library.descr cmLib
                         val cmLibOSString = CM.Library.osstring cmLib

                         fun mlbLibDef () =
                            let
                               val {base, ext} = OS.Path.splitBaseExt cmLibOSString
                               val mlbLib = OS.Path.joinBaseExt {base = base, ext = SOME "mlb"}
                            in
                               mlbLib
                            end
                                  
                         fun doitAnchoredPath (anchor, path) =
                            case peekAnchorMap anchor of
                               SOME mlbPath => 
                                  let
                                     val {dir, file} = OS.Path.splitDirFile path
                                     val {base, ext} = OS.Path.splitBaseExt file
                                     val file = OS.Path.joinBaseExt {base = base, ext = SOME "mlb"}
                                     val path = OS.Path.joinDirFile {dir = dir, file = file}
                                     val mlbLib = OS.Path.joinDirFile {dir = mlbPath, file = path}
                                  in 
                                     concat ["(* ", cmLibDescr, " ====> *) ", mlbLib]
                                  end
                             | NONE => 
                                  concat ["(* ", cmLibDescr, " =??=> *) ", mlbLibDef ()]

                         val mlbLib =
                            if String.sub (cmLibDescr, 0) = #"$"
                               then case String.fields (fn #"/" => true | _ => false) cmLibDescr of
                                       ["$", abbrev] =>
                                          let
                                             val anchor = OS.Path.base abbrev
                                             val path = abbrev
                                          in
                                             doitAnchoredPath (anchor, path)
                                          end
                                     | anchor::path =>
                                          let
                                             val anchor = String.extract (anchor, 1, NONE)
                                             val path = String.concatWith "/" path
                                          in
                                             doitAnchoredPath (anchor, path)
                                          end
                                     | _ => die "strange anchored path"
                               else concat ["(* ", cmLibOSString, " ===> *) ", mlbLibDef ()]
                      in
                         concat 
                         ["  basis ", bid, " = \n",
                          "    bas\n",
                          "      ", mlbLib, "\n",
                          "    end\n"]
                      end)
                     (imports, importLibs)
               in
                  TextIO.output (out, "local\n");
                  List.app (fn s => TextIO.output (out, s)) imports;
                  TextIO.output (out, "in\n");
                  GenMLB.gen {graph = graph,
                              nativesrc = nativesrc,
                              importprefix = fn _ => "",
                              exportprefix = "",
                              outstream = out};
                  TextIO.output (out, "end\n")
               end
          | NONE => ()
      end

   fun usage msg =
      (message "Usage: cm2mlb [-Dsym ...] [-map file] sources.cm"
       ; die msg)

   fun main (_, args) =
      let
         val defines = ref ["MLton"]
         val maps = ref []
         fun loop args = 
            case args of
               [file] =>
                  cm2mlb {defines = !defines,
                          maps = !maps,
                          out = TextIO.stdOut,
                          sources = file}
             | flag :: args =>
                  if String.isPrefix "-D" flag
                     then
                        (defines := String.extract (flag, 2, NONE) :: !defines
                         ; loop args)
                  else if "-map" = flag
                     then case args of
                            file :: args => (maps := file :: !maps
                                             ; loop args)
                          | _ => usage "missing map file"
                  else usage (String.concat ["invalid flag ", flag])
             | _ => usage "wrong number of arguments"
      in
         loop args handle e => die (concat ["cm2mlb failed: ", General.exnMessage e])
         ; 0
      end

   fun export () =
      SMLofNJ.exportFn
      ("cm2mlb", main)
end
