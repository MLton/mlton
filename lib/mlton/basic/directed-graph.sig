type int = Int.t

structure DotColor =
   struct
      datatype t =
	 (* (Hue, Saturation, Brightness).  All between 0 and 1. *)
	 HSB of real * real * real
       | Aliceblue
       | Antiquewhite1 | Antiquewhite2 | Antiquewhite3 | Antiquewhite4
       | Aquamarine1 | Aquamarine2 | Aquamarine3 | Aquamarine4
       | Azure1 | Azure2 | Azure3 | Azure4
       | Beige
       | Bisque1 | Bisque2 | Bisque3 | Bisque4
       | Black
       | Blanchedalmond
       | Blue1 | Blue2 | Blue3 | Blue4
       | Blueviolet
       | Brown1 | Brown2 | Brown3 | Brown4
       | Burlywood1 | Burlywood2 | Burlywood3 | Burlywood4
       | Cadetblue1 | Cadetblue2 | Cadetblue3 | Cadetblue4
       | Chartreuse1 | Chartreuse2 | Chartreuse3 | Chartreuse4
       | Chocolate1 | Chocolate2 | Chocolate3 | Chocolate4
       | Coral1 | Coral2 | Coral3 | Coral4
       | Corn
       | Cornsilk1 | Cornsilk2 | Cornsilk3 | Cornsilk4
       | Crimson
       | Cyan1 | Cyan2 | Cyan3 | Cyan4
       | Darkgoldenrod1 | Darkgoldenrod2 | Darkgoldenrod3 | Darkgoldenrod4
       | Darkgreen
       | Darkkhaki
       | Darkolivegreen1 | Darkolivegreen2 | Darkolivegreen3 | Darkolivegreen4
       | Darkorange1 | Darkorange2 | Darkorange3 | Darkorange4
       | Darkorchid1 | Darkorchid2 | Darkorchid3 | Darkorchid4
       | Darksalmon
       | Darkseagreen1 | Darkseagreen2 | Darkseagreen3 | Darkseagreen4
       | Darkslateblue
       | Darkslategray1 | Darkslategray2 | Darkslategray3 | Darkslategray4
       | Darkturquoise
       | Darkviolet
       | Deeppink1 | Deeppink2 | Deeppink3 | Deeppink4
       | Deepskyblue1 | Deepskyblue2 | Deepskyblue3 | Deepskyblue4
       | Dimgray
       | Dodgerblue1 | Dodgerblue2 | Dodgerblue3 | Dodgerblue4
       | Forestgreen
       | Gainsboro
       | Ghostwhite
       | Gold1 | Gold2 | Gold3 | Gold4
       | Goldenrod1 | Goldenrod2 | Goldenrod3 | Goldenrod4
       | Gray
       | Gray0 | Gray1 | Gray2 | Gray3 | Gray4 | Gray5 | Gray6 | Gray7 | Gray8
       | Gray9 | Gray10 | Gray11 | Gray12 | Gray13 | Gray14 | Gray15 | Gray16
       | Gray17 | Gray18 | Gray19 | Gray20 | Gray21 | Gray22 | Gray23 | Gray24
       | Gray25 | Gray26 | Gray27 | Gray28 | Gray29 | Gray30 | Gray31 | Gray32
       | Gray33 | Gray34 | Gray35 | Gray36 | Gray37 | Gray38 | Gray39 | Gray40
       | Gray41 | Gray42 | Gray43 | Gray44 | Gray45 | Gray46 | Gray47 | Gray48
       | Gray49 | Gray50 | Gray51 | Gray52 | Gray53 | Gray54 | Gray55 | Gray56
       | Gray57 | Gray58 | Gray59 | Gray60 | Gray61 | Gray62 | Gray63 | Gray64
       | Gray65 | Gray66 | Gray67 | Gray68 | Gray69 | Gray70 | Gray71 | Gray72
       | Gray73 | Gray74 | Gray75 | Gray76 | Gray77 | Gray78 | Gray79 | Gray80
       | Gray81 | Gray82 | Gray83 | Gray84 | Gray85 | Gray86 | Gray87 | Gray88
       | Gray89 | Gray90 | Gray91 | Gray92 | Gray93 | Gray94 | Gray95 | Gray96
       | Gray97 | Gray98 | Gray99 | Gray100
       | Green1 | Green2 | Green3 | Green4
       | Greenyellow
       | Honeydew1 | Honeydew2 | Honeydew3 | Honeydew4
       | Hotpink1 | Hotpink2 | Hotpink3 | Hotpink4
       | Indianred1 | Indianred2 | Indianred3 | Indianred4
       | Indigo
       | Ivory1 | Ivory2 | Ivory3 | Ivory4
       | Khaki1 | Khaki2 | Khaki3 | Khaki4
       | Lavender
       | Lavenderblush1 | Lavenderblush2 | Lavenderblush3 | Lavenderblush4
       | Lawngreen
       | Lemonchi
       | Lightblue1 | Lightblue2 | Lightblue3 | Lightblue4
       | Lightcyan1 | Lightcyan2 | Lightcyan3 | Lightcyan4
       | Lightgoldenrod1 | Lightgoldenrod2 | Lightgoldenrod3 | Lightgoldenrod4
       | Lightgoldenrodyellow
       | Lightgray
       | Lightpink1 | Lightpink2 | Lightpink3 | Lightpink4
       | Lightsalmon1 | Lightsalmon2 | Lightsalmon3 | Lightsalmon4
       | Lightseagreen
       | Lightskyblue1 | Lightskyblue2 | Lightskyblue3 | Lightskyblue4
       | Lightslateblue1 | Lightslateblue2 | Lightslateblue3 | Lightslateblue4
       | Lightslategray
       | Lightyellow1 | Lightyellow2 | Lightyellow3 | Lightyellow4
       | Limegreen
       | Linen
       | Magenta1 | Magenta2 | Magenta3 | Magenta4
       | Maroon1 | Maroon2 | Maroon3 | Maroon4
       | Mediumaquamarine
       | Mediumblue
       | Mediumorchid1 | Mediumorchid2 | Mediumorchid3 | Mediumorchid4
       | Mediumpurple1 | Mediumpurple2 | Mediumpurple3 | Mediumpurple4
       | Mediumseagreen
       | Mediumslateblue
       | Mediumspringgreen
       | Mediumturquoise
       | Mediumvioletred
       | Midnightblue
       | Mintcream
       | Mistyrose1 | Mistyrose2 | Mistyrose3 | Mistyrose4
       | Moccasin
       | Navajowhite1 | Navajowhite2 | Navajowhite3 | Navajowhite4
       | Navy
       | Navyblue
       | Oldlace
       | Olivedrab1 | Olivedrab2 | Olivedrab3 | Olivedrab4
       | On1 | On2 | On3 | On4
       | Oralwhite
       | Orange1 | Orange2 | Orange3 | Orange4
       | Orangered1 | Orangered2 | Orangered3 | Orangered4
       | Orchid1 | Orchid2 | Orchid3 | Orchid4
       | Owerblue
       | Palegoldenrod
       | Palegreen1 | Palegreen2 | Palegreen3 | Palegreen4
       | Paleturquoise1 | Paleturquoise2 | Paleturquoise3 | Paleturquoise4
       | Palevioletred1 | Palevioletred2 | Palevioletred3 | Palevioletred4
       | Papayawhip
       | Peachpu1 | Peachpu2 | Peachpu3 | Peachpu4
       | Peru
       | Pink1 | Pink2 | Pink3 | Pink4
       | Plum1 | Plum2 | Plum3 | Plum4
       | Powderblue
       | Purple1 | Purple2 | Purple3 | Purple4
       | Rebrick1 | Rebrick2 | Rebrick3 | Rebrick4
       | Red1 | Red2 | Red3 | Red4
       | Rosybrown1 | Rosybrown2 | Rosybrown3 | Rosybrown4
       | Royalblue1 | Royalblue2 | Royalblue3 | Royalblue4
       | Saddlebrown
       | Salmon1 | Salmon2 | Salmon3 | Salmon4
       | Sandybrown
       | Seagreen1 | Seagreen2 | Seagreen3 | Seagreen4
       | Seashell1 | Seashell2 | Seashell3 | Seashell4
       | Sienna1 | Sienna2 | Sienna3 | Sienna4
       | Skyblue1 | Skyblue2 | Skyblue3 | Skyblue4
       | Slateblue1 | Slateblue2 | Slateblue3 | Slateblue4
       | Slategray1 | Slategray2 | Slategray3 | Slategray4
       | Snow1 | Snow2 | Snow3 | Snow4
       | Springgreen1 | Springgreen2 | Springgreen3 | Springgreen4
       | Steelblue1 | Steelblue2 | Steelblue3 | Steelblue4
       | Tan1 | Tan2 | Tan3 | Tan4
       | Thistle1 | Thistle2 | Thistle3 | Thistle4
       | Tomato1 | Tomato2 | Tomato3 | Tomato4
       | Turquoise1 | Turquoise2 | Turquoise3 | Turquoise4
       | Violet
       | Violetred1 | Violetred2 | Violetred3 | Violetred4
       | Wheat1 | Wheat2 | Wheat3 | Wheat4
       | White
       | Whitesmoke
       | Yellow1 | Yellow2 | Yellow3 | Yellow4
       | Yellowgreen
   end

signature DIRECTED_GRAPH = 
   sig
      structure Node: 
	 sig
	    type edge
	    type t

	    val equals: t * t -> bool
	    val hasEdge: {from: t, to: t} -> bool
	    val layout: t -> Layout.t
	    val plist: t -> PropertyList.t
	    val successors: t -> edge list
	 end
      structure Edge:
	 sig
	    type t

	    val equals: t * t -> bool
	    val plist: t -> PropertyList.t
	    val to: t -> Node.t
	 end
      sharing type Node.edge = Edge.t

      (* depth first search *)
      structure DfsParam:
	 sig
	    type t = {startNode: Node.t -> unit,
		      finishNode: Node.t -> unit,
		      handleTreeEdge: Edge.t -> unit,
		      handleNonTreeEdge: Edge.t -> unit,
		      startTree: Node.t -> unit,
		      finishTree: Node.t -> unit,
		      finishDfs: unit -> unit}
	    val finishNode: (Node.t -> unit) -> t
	    val ignore: 'a -> unit
	    val combine: t * t -> t
	 end

      (* the main graph type *)
      type t

      val addEdge: t * {from: Node.t, to: Node.t} -> Edge.t
      val dfs: t * DfsParam.t -> unit
      val dfsNodes: t * Node.t list * DfsParam.t -> unit
      val discoverFinishTimes: t -> {discover: Node.t -> int,
				     finish: Node.t -> int,
				     destroy: unit -> unit,
				     param: DfsParam.t}
      val display:
	 {graph: t,
	  layoutNode: Node.t -> Layout.t,
	  display: Layout.t -> unit} -> unit
      (* dominators {graph, root}
       * Pre: All nodes in graph must be reachable from root.
       *      This condition is checked.
       *)
      val dominators:
	 {graph: t,
	  root: Node.t}
	 ->
	 (* The immediate dominator of each node.
	  * The root's immediate dominator is itself.
	  *)
	 {idom: Node.t -> Node.t}
      val dominatorTree:
	 {graph: t, root: Node.t}
	 -> {graphToTree: Node.t -> Node.t,
	     tree: t}
      val foreachDescendent: t * Node.t * (Node.t -> unit) -> unit
      val foreachEdge: t * (Node.t * Edge.t -> unit) -> unit
      val foreachNode: t * (Node.t -> unit) -> unit
(*      exception Input *)
(*     val input: In.t * (In.t -> 'a)* (In.t -> 'b) -> t * 'a * (Edge.t -> 'b) *)
(*      val isCyclic: t -> bool*)
(*      val layoutDaVinci: t * (Node.t -> Layout.t) -> Layout.t *)
      val new: unit -> t
      val newNode: t -> Node.t
      val nodes: t -> Node.t list
(*      val random: {numNodes: int, numEdges: int} -> t*)
(*      val removeBackEdges: t -> unit *)
      (* removeEdge fails if edge isn't there. *)
(*      val removeEdge: t * Edge.t -> unit *)
      (* Strongly-connected components.
       * Each component is given as a list of nodes.
       * The components are returned topologically sorted.
       *)
      val stronglyConnectedComponents: t -> Node.t list list
      exception TopologicalSort
      val topologicalSort: t -> Node.t list
(*      val transpose: t -> t *)

      structure LayoutDot:
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
		   | Rank of rank * Node.t list
		   | RankDir of rankDir
		   | RankSep of real (* inches *)
		   | Ratio of ratio
		   | Size of {height: real, width: real} (* inches *)
	       end
	    
	    val layout: {graph: t,
			 title: string,
			 options: GraphOption.t list,
			 edgeOptions: Edge.t -> EdgeOption.t list,
			 nodeOptions: Node.t -> NodeOption.t list} -> Layout.t
	 end
   end


functor TestDirectedGraph (S: DIRECTED_GRAPH): sig end =
struct

open S

(* Section 7.3 of Muchnick. *)
local
   val g = new ()
   val {get = name, set = setName} =
      Property.getSetOnce (Node.plist,
			   Property.initRaise ("name", Node.layout))
   val node = String.memoize (fn s =>
			      let
				 val n = newNode g
				 val _ = setName (n, s)
			      in n
			      end)
   val _ =
      List.foreach ([("entry\nfoo", "B1"),
		     ("B1", "B2"),
		     ("B1", "B3"),
		     ("B2", "exit"),
		     ("B3", "B4"),
		     ("B4", "B5"),
		     ("B4", "B6"),
		     ("B5", "exit"),
		     ("B6", "B4")], fn (from, to) =>
		    (addEdge (g, {from = node from, to = node to})
		     ; ()))
   val _ =
      File.withOut
      ("/tmp/z.dot", fn out =>
       let
	  open LayoutDot
       in
	  Layout.output (layout
			 {graph = g,
			  title = "Muchnick",
			  options = [],
			  edgeOptions = fn _ => [],
			  nodeOptions = fn n => [NodeOption.label (name n)]},
			 out)
	  ; Out.newline out
       end)
   val {idom} = dominators {graph = g, root = node "entry\nfoo"}
   val g2 = new ()
   val {get = oldNode, set = setOldNode} =
      Property.getSetOnce (Node.plist,
			   Property.initRaise ("oldNode", Node.layout))
   val {get = newNode} =
      Property.get (Node.plist,
		    Property.initFun (fn n =>
				      let
					 val n' = newNode g2
					 val _ = setOldNode (n', n)
				      in n'
				      end))
   val _ = foreachNode (g, fn n =>
			(addEdge (g2, {from = newNode (idom n),
				       to = newNode n})
			 ; ()))
   val _ =
      File.withOut
      ("/tmp/z2.dot", fn out =>
       let
	  open LayoutDot
       in
	  Layout.output
	  (layout {graph = g2,
		   title = "dom",
		   options = [],
		   edgeOptions = fn _ => [],
		   nodeOptions = fn n => [NodeOption.label (name (oldNode n))]},
	   out)
	  ; Out.newline out
       end)
in
end

end
