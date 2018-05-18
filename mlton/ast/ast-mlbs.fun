(* Copyright (C) 2017-2018 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AstMLBs (S: AST_MLBS_STRUCTS): AST_MLBS = 
struct

open S

structure AstPrograms = AstPrograms (S)

open AstPrograms Layout

fun mkCtxt (x, lay) () =
   seq [str "in: ", lay x]

val layouts = List.map
structure Wrap = Region.Wrap
val node = Wrap.node

(*---------------------------------------------------*)
(*                Basdecs and Basexps                *)
(*---------------------------------------------------*)

datatype basexpNode =
   Bas of basdec
 | Let of basdec * basexp
 | Var of Basid.t
and basdecNode =
   Ann of string * Region.t * basdec
 | Basis of {name: Basid.t, def: basexp} vector
 | Defs of ModIdBind.t
 | Local of basdec * basdec
 | MLB of {fileAbs: File.t, fileUse: File.t} * basdec Promise.t
 | Open of Basid.t vector   
 | Prim
 | Prog of {fileAbs: File.t, fileUse: File.t} * Program.t Promise.t
 | Seq of basdec list
withtype basexp = basexpNode Wrap.t
     and basdec = basdecNode Wrap.t

fun layoutBasexp exp =
   case node exp of
      Bas dec => align [str "bas", indent (layoutBasdec dec, 3), str "end"]
    | Let (dec, exp) => Pretty.lett (layoutBasdec dec, layoutBasexp exp)
    | Var basid => Basid.layout basid 
and layoutBasdec dec =
   case node dec of
      Ann (anns,_, dec) =>
         align [str "ann", 
                indent (seq [str String.dquote, str anns, str String.dquote], 3),
                str "in", 
                indent (layoutBasdec dec, 3), 
                str "end"]
    | Basis basbnds =>
         layoutAndsBind
         ("basis", "=", basbnds, fn {name, def} =>
          (case node def of Var _ => OneLine | _ => Split 3,
           Basid.layout name, layoutBasexp def))
    | Defs def => ModIdBind.layout def
    | Local (dec1, dec2) => Pretty.locall (layoutBasdec dec1, layoutBasdec dec2) 
    | MLB ({fileUse, ...}, _) => File.layout fileUse
    | Open bs => seq [str "open ",
                      seq (separate (Vector.toListMap (bs, Basid.layout),
                                     " "))] 
    | Prim => str "_prim"
    | Prog ({fileUse, ...}, _) => File.layout fileUse
    | Seq decs => align (layoutBasdecs decs)
and layoutBasdecs decs = layouts (decs, layoutBasdec)

fun checkSyntaxBasexp (e: basexp): unit =
   case node e of
      Bas dec => checkSyntaxBasdec dec
    | Let (dec, exp) => (checkSyntaxBasdec dec
                         ; checkSyntaxBasexp exp)
    | Var _ => ()
and checkSyntaxBasdec (d: basdec): unit =
   case node d of
      Ann (_, _, dec) => checkSyntaxBasdec dec
    | Basis basbnds =>
         reportDuplicates
         (basbnds, {ctxt = mkCtxt (d, layoutBasdec),
                    equals = (fn ({name = id, ...}, {name = id', ...}) =>
                              Basid.equals (id, id')),
                    layout = Basid.layout o #name,
                    name = "basis definition",
                    region = Basid.region o #name})
    | Defs def => ModIdBind.checkSyntax def
    | Local (dec1, dec2) => 
         (checkSyntaxBasdec dec1
          ; checkSyntaxBasdec dec2)
    | MLB _ => ()
    | Open _ => ()
    | Prim => ()
    | Prog _ => ()
    | Seq decs => List.foreach (decs, checkSyntaxBasdec)

fun sourceFiles (d: basdec): File.t vector =
   let
      val sourceFiles : File.t Buffer.t =
         Buffer.new {dummy = "<dummy>"}
      val psi : File.t -> bool ref =
         String.memoize (fn _ => ref false)

      fun sourceFilesBasexp (e: basexp): unit =
         case node e of
            Bas dec => sourceFilesBasdec dec
          | Let (dec, exp) => (sourceFilesBasdec dec
                               ; sourceFilesBasexp exp)
          | Var _ => ()
      and sourceFilesBasdec (d: basdec): unit =
         case node d of
            Ann (_, _, dec) => sourceFilesBasdec dec
          | Basis basbnds =>
               Vector.foreach
               (basbnds, fn {def, ...} =>
                sourceFilesBasexp def)
          | Defs _ => ()
          | Local (dec1, dec2) => (sourceFilesBasdec dec1
                                   ; sourceFilesBasdec dec2)
          | MLB ({fileAbs, fileUse, ...}, dec) =>
               let
                  val b = psi fileAbs
               in
                  if !b
                     then ()
                     else let
                             val () = b := true
                          in
                             Buffer.add (sourceFiles, fileUse)
                             ; sourceFilesBasdec (Promise.force dec)
                          end
               end
          | Open _ => ()
          | Prim => ()
          | Prog ({fileUse, ...}, _) => Buffer.add (sourceFiles, fileUse)
          | Seq decs => List.foreach (decs, sourceFilesBasdec)
      val () = sourceFilesBasdec d
   in
      Buffer.toVector sourceFiles
   end
val sourceFiles =
   Trace.trace 
   ("AstMLBs.sourceFiles", Layout.ignore, Vector.layout File.layout)
   sourceFiles


structure Basexp =
   struct
      open Wrap
      type basdec = basdec
      type t = basexp
      datatype node = datatype basexpNode
      type node' = node
      type obj = t

      val layout = layoutBasexp
   end

structure Basdec =
   struct
      open Wrap
      type t = basdec
      datatype node = datatype basdecNode
      type node' = node
      type obj = t

      fun make n = makeRegion (n, Region.bogus)
      val seq = make o Seq
      val empty = seq []
      val checkSyntax = checkSyntaxBasdec
      val layout = layoutBasdec
      val sourceFiles = sourceFiles
   end
end
