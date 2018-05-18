(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature DOT =
   sig
      datatype color = datatype DotColor.t
      datatype direction =
         Backward
       | Both
       | Forward
       | None
      datatype fontFamily =
         Courier
       | Helvetica
       | Symbol
       | Times
      datatype fontWeight =
         Bold
       | Italic
       | Roman
      type fontName = fontFamily * fontWeight
      datatype justify =
         Center
       | Left
       | Right
      datatype orientation =
         Landscape
       | Portrait
      datatype polygonOption =
         Distortion of real (* -1.0 <= r <= 1.0 *)
       | Orientation of int (* 0 <= i <= 360.  Clockwise rotation from
                             * X axis in degrees.
                             *)
       | Peripheries of int
       | Skew of real (* -1.0 <= r <= 1.0 *)
      datatype rank = Max | Min | Same
      datatype rankDir =
         LeftToRight
       | TopToBottom
      datatype ratio =
         Auto
       | Compress
       | Fill
       | WidthOverHeight of real
      datatype shape =
         Box
       | Circle
       | Diamond
       | Ellipse
       | Plaintext
       | Polygon of {sides: int,
                     options: polygonOption list}
      datatype style =
         BoldStyle
       | Dashed
       | Dotted
       | Filled
       | Invisible
       | Solid
      structure EdgeOption:
         sig
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

            val label: string -> t (* label s = Label (s, Center) *)
         end
      structure NodeOption:
         sig
            datatype t =
               Color of color
             | FontColor of color
             | FontName of fontName
             | FontSize of int (* points *)
             | Height of real (* inches *)
             | Label of (string * justify) list
             | Shape of shape
             | Width of real (* inches *)

            val label: string -> t (* label s = Label (s, Center) *)
         end
      structure GraphOption:
         sig
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
             | Nslimit of int (* network simplex limit *)
             | Orientation of orientation
             | Page of {height: real, width: real} (* inches *)
             | Rank of rank * {nodeName: string} list
             | RankDir of rankDir
             | RankSep of real (* inches *)
             | Ratio of ratio
             | Size of {height: real, width: real} (* inches *)
         end

      val layout: {nodes: {name: string,
                           options: NodeOption.t list,
                           successors: {name: string,
                                        options: EdgeOption.t list} list
                           } list,
                   options: GraphOption.t list,
                   title: string} -> Layout.t
   end
