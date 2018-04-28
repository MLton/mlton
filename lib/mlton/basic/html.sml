(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Html:> HTML = 
struct

fun tag (name: string,
        attributes: (string * string) list,
        body: Layout.t) =
   let open Layout
   in seq [str "<", str name, 
           seq (List.map (attributes, fn (name, value) =>
                          str (concat [" ", name, " = ", value]))),
           str ">",
           body,
           str (concat ["</", name, ">"])]
   end

structure Align =
   struct
      datatype t = Left | Center | Right

      fun toString a =
         case a of
            Left => "left"
          | Center => "center"
          | Right => "right"

      fun attribute a = ("align", toString a)
   end

structure Element =
   struct
      datatype tableOption =
         Border of int
       | CellPadding of int
       | CellSpacing of int

      datatype t =
         A of Url.t * t
       | Br
       | H1 of Align.t * t
       | Img of {src: Url.t}
       | P of Align.t * t
       | Pre of t
       | Seq of t list
       | String of string
       | Table of tableOption list * t list list
       | Tt of t

      val a = A
      val br = Br
      val h1 = H1
      val img = Img
      val p = P
      val pre = Pre
      val seq = Seq
      val str = String
      val table = Table
      val tt = Tt

      fun layoutAe ((a, e), s) = tag (s, [Align.attribute a], layout e) 
      and layout e =
         let open Layout
         in case e of
            A (u, e) => tag ("A", [("href", Url.toString u)], layout e)
          | Br => align [empty, tag ("BR", [], empty)]
          | H1 ae => layoutAe (ae, "H1")
          | Img {src, ...} => tag ("IMAGE", [("src", Url.toString src)], empty)
          | P ae => layoutAe (ae, "P")
          | Pre t => tag ("PRE", [], layout t)
          | Seq es => seq (List.map (es, layout))
          | String s => str s
          | Table (options, rows) =>
               tag ("TABLE",
                   List.map (options,
                            fn Border n => ("BORDER", Int.toString n)
                             | CellPadding n => ("CELLPADDING", Int.toString n)
                             | CellSpacing n => ("CELLSPACING", Int.toString n)),
                   seq (List.map (rows, fn cols =>
                                tag ("TR", [],
                                    seq (List.map (cols, fn c =>
                                                 tag ("TH", [], layout c)))))))
          | Tt t => tag ("TT", [], layout t)
         end
   end

structure Option =
   struct
      datatype t =
         Redirect of {seconds: int,
                      uri: Url.t}
       | Title of string

      fun layout (opt: t): Layout.t =
         case opt of
            Redirect {seconds, uri} =>
               tag ("META", [("HTTP-EQUIV", "Refresh"),
                            ("Content",
                             concat [String.dquote,
                                    Int.toString seconds,
                                    "; URL=", Url.toString uri,
                                    String.dquote])],
                   Layout.empty)
          | Title s => tag ("TITLE", [], Layout.str s)
   end

datatype t =
   T of {options: Option.t list,
         body: Element.t}

fun layout (T {options, body}) =
   let open Layout
   in align
      [str "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">",
       tag ("HTML", [],
           align [tag ("HEAD", [], align (List.map (options, Option.layout))),
                 tag ("BODY", [], Element.layout (body))])]
   end

end
