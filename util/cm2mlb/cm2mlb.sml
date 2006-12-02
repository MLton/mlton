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

                         fun doitAnchoredPath arcs =
                            let
                               fun loop (prefix, suffix) =
                                  if List.null prefix 
                                     then concat ["(* ", cmLibDescr, " =??=> *) ", mlbLibDef ()]
                                     else case peekAnchorMap (String.concatWith "/" (List.rev prefix)) of
                                             SOME mlbPath =>
                                                concat ["(* ", cmLibDescr, " ====> *) ", mlbPath ^ suffix]
                                           | NONE =>
                                                let
                                                   val suffix =
                                                      if suffix = ""
                                                         then OS.Path.joinBaseExt
                                                              {base = #base (OS.Path.splitBaseExt (List.hd prefix)),
                                                               ext = SOME "mlb"}
                                                         else (List.hd prefix) ^ suffix
                                                in
                                                   loop (List.tl prefix, "/" ^ suffix)
                                                end
                            in
                               loop (List.rev arcs, "")
                            end

                         val mlbLib =
                            if String.sub (cmLibDescr, 0) = #"$"
                               then case String.fields (fn #"/" => true | _ => false) cmLibDescr of
                                       "$" :: (arcs as (arc0 :: _)) => 
                                          doitAnchoredPath (("$" ^ arc0) :: arcs)
                                     | arc0 :: arcs =>
                                          let
                                             val arc0 =
                                                case CharVector.findi (fn (_, #"(") => true | _ => false) arc0 of
                                                   SOME (i, _) => 
                                                      String.extract (arc0, i + 2, SOME (String.size arc0 - i - 3))
                                                 | NONE => arc0
                                          in 
                                             doitAnchoredPath (arc0 :: arcs)
                                          end
                                     | arcs => doitAnchoredPath arcs
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
