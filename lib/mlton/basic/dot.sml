(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Dot: DOT =
struct

fun escapeString s =
   String.translate
   (s, fn c =>
    if Char.isPrint c
       then (case c of
                #"\"" => "\\\""
 | #"\\" => "\\\\\\\\"
 | _ => Char.toString c)
    else
       case c of
          #"\n" => "\\\\\\\\n"
        | #"\t" => "\\\\\\\\t"
        | c => concat ["\\\\\\\\", Int.format (Char.ord c, StringCvt.OCT)])

val dquote = "\""
fun quote s = concat [dquote, s, dquote]
fun lab (name, value) = concat [name, " = ", quote value]
fun optionsToString (opts: 'a list, toString: 'a -> string, sep): string =
   concat (List.separate (List.map (opts, toString), concat [sep, " "]))
fun layoutOptions z = Layout.str (optionsToString z)
val boolToString = Bool.toString
val intToString = Int.toString
val realToString = DotColor.realToString
fun real2ToString (x, y) = concat [realToString x, ", ", realToString y]

structure Color = DotColor
datatype color = datatype Color.t

datatype direction =
   Backward
    | Both
    | Forward
    | None

val directionToString =
   fn Backward => "backward"
    | Both => "both"
    | Forward => "forward"
    | None => "none"

datatype fontFamily =
   Courier
    | Helvetica
    | Symbol
    | Times

val fontFamilyToString =
   fn Courier => "Courier"
    | Helvetica => "Helvetica"
    | Symbol => "Symbol"
    | Times => "Times"

datatype fontWeight =
   Bold
    | Italic
    | Roman

val fontWeightToString =
   fn Bold => "Bold"
    | Italic => "Italic"
    | Roman => "Roman"

type fontName = fontFamily * fontWeight

fun fontNameToString (f, w) =
   concat [fontFamilyToString f, "-", fontWeightToString w]

datatype justify =
   Center
  | Left
  | Right

val justifyToString =
   fn Center => "\\n"
    | Left => "\\l"
    | Right => "\\r"

datatype orientation =
   Landscape
    | Portrait

val orientationToString =
   fn Landscape => "landscape"
    | Portrait => "portrait"

datatype polygonOption =
   Distortion of real
    | Orientation of int
    | Peripheries of int
    | Skew of real

fun polygonOptionToString opt =
   lab (case opt of
           Distortion r => ("distortion", realToString r)
         | Orientation i => ("orientation", intToString i)
         | Peripheries p => ("peripheries", intToString p)
         | Skew s => ("skew", realToString s))

datatype rank = Max | Min | Same

val rankToString =
   fn Max => "max"
    | Min => "min"
    | Same => "same"

datatype rankDir =
   LeftToRight
    | TopToBottom

val rankDirToString =
   fn LeftToRight => "LR"
    | TopToBottom => "TB"

datatype ratio =
   Auto
    | Compress
    | Fill
    | WidthOverHeight of real

val ratioToString =
   fn Auto => "auto"
    | Compress => "compress"
    | Fill => "fill"
    | WidthOverHeight r => realToString r

datatype shape =
   Box
    | Circle
    | Diamond
    | Ellipse
    | Plaintext
    | Polygon of {sides: int,
                  options: polygonOption list}

val shapeToString =
   fn Box => "box"
    | Circle => "circle"
    | Diamond => "diamond"
    | Ellipse => "ellipse"
    | Plaintext => "plaintext"
    | Polygon {sides, options} =>
         concat
         ["polygon, ",
          lab ("sides", intToString sides),
          case options of
             [] => ""
           | _ => concat [", ",
                          optionsToString (options,
                                           polygonOptionToString,
                                           ",")]]

datatype style =
   BoldStyle
    | Dashed
    | Dotted
    | Filled
    | Invisible
    | Solid

val styleToString =
   fn BoldStyle => "bold"
    | Dashed => "dashed"
    | Dotted => "dotted"
    | Filled => "filled"
    | Invisible => "invis"
    | Solid => "solid"

fun labelToString (l: (string * justify) list): string =
   concat (List.concatMap (l, fn (s, j) =>
                           [escapeString s, justifyToString j]))

structure EdgeOption =
   struct
      datatype t =
         Color of color
       | Decorate of bool (* connect edge label to edge *)
       | Dir of direction
       | FontColor of color
       | FontName of fontName
       | FontSize of int (* points *)
       | Label of (string * justify) list
       | Minlen of int
       | Style of style
       | Weight of int

      fun label s = Label [(s, Center)]

      fun toString opt =
         lab (case opt of
                 Color c => ("color", Color.toString c)
               | Decorate d => ("decorate", boolToString d)
               | Dir d => ("dir", directionToString d)
               | FontColor c => ("fontcolor", Color.toString c)
               | FontName n => ("fontname", fontNameToString n)
               | FontSize s => ("fontsize", intToString s)
               | Label l => ("label", labelToString l)
               | Minlen n => ("minlen", intToString n)
               | Style s => ("style", styleToString s)
               | Weight n => ("weight", intToString n))
   end

structure NodeOption =
   struct
      datatype t =
         Color of color
       | FontColor of color
       | FontName of fontName
       | FontSize of int (* points *)
       | Height of real (* inches *)
       | Label of (string * justify) list
       | Shape of shape
       | Width of real (* inches *)

      fun label s = Label [(s, Center)]

      fun toString opt =
         lab (case opt of
                 Color c => ("color", Color.toString c)
               | FontColor c => ("fontcolor", Color.toString c)
               | FontName n => ("fontname", fontNameToString n)
               | FontSize s => ("fontsize", intToString s)
               | Height r => ("height", realToString r)
               | Label l => ("label", labelToString l)
               | Shape s => ("shape", shapeToString s)
               | Width r => ("width", realToString r))
   end

structure GraphOption =
   struct
      datatype t =
         Center of bool
       | Color of color (* *)
       | Concentrate of bool
       | FontColor of color
       | FontName of fontName
       | FontSize of int (* points *)
       | Label of string
       | Margin of real * real (* inches *)
       | Mclimit of real (* mincross iterations multiplier *)
       | NodeSep of real (* inches *)
       | Nslimit of int
       | Orientation of orientation
       | Page of {height: real, width: real} (* inches *)
       | Rank of rank * {nodeName: string} list
       | RankDir of rankDir
       | RankSep of real (* inches *)
       | Ratio of ratio
       | Size of {height: real, width: real} (* inches *)

      fun toString opt =
         case opt of
            Center x => lab ("center", boolToString x)
          | Color x => lab ("color", Color.toString x)
          | Concentrate x => lab ("concentrate", boolToString x)
          | FontColor x => lab ("fontcolor", Color.toString x)
          | FontName x => lab ("fontname", fontNameToString x)
          | FontSize x => lab ("fontsize", intToString x)
          | Label x => lab ("label", escapeString x)
          | Margin x => lab ("margin", real2ToString x)
          | Mclimit x => lab ("mclimit", realToString x)
          | NodeSep x => lab ("nodesep", realToString x)
          | Nslimit n => lab ("nslimit", intToString n)
          | Orientation x => lab ("orientation", orientationToString x)
          | Page {height, width} =>
               lab ("page", real2ToString (width, height))
          | RankDir x => lab ("rankdir", rankDirToString x)
          | Rank (r, ns) =>
               concat ["{ ",
                       lab ("rank ", rankToString r),
                       "; ",
                       concat (List.revMap (ns, fn {nodeName} =>
                                            concat [nodeName, " "])),
                       "}"]
          | RankSep x => lab ("ranksep", realToString x)
          | Ratio x => lab ("ratio", ratioToString x)
          | Size {height, width} =>
               lab ("size", real2ToString (width, height))
   end

fun layout {options, nodes, title: string} =
   let
      open Layout
   in
      align
      [str (concat ["digraph \"", title, "\" {"]),
       layoutOptions (GraphOption.Label title :: options,
                      GraphOption.toString, ";"),
       align (List.revMap
              (nodes, fn {name = from, options, successors} =>
               align
               [seq [str from,
                     str " [",
                     layoutOptions (options, NodeOption.toString, ","),
                     str "]"],
                align (List.revMap
                       (successors, fn {name = to, options} =>
                        seq [str (concat [from, " -> ", to, " ["]),
                             layoutOptions (options, EdgeOption.toString, ","),
                             str "]"]))])),
       str "}"]
   end

end
