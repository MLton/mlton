type int = Int.t
   
signature HTML = 
   sig
      structure Align:
	 sig
	    datatype t = Left | Center | Right
	 end
      structure Element:
	 sig
	    type t

	    datatype tableOption =
	       Border of int
	     | CellPadding of int
	     | CellSpacing of int

	    val a: Uri.t * t -> t
	    val br: t
	    val img: {src: Uri.t} -> t
	    val layout: t -> Layout.t
	    val pre: t -> t
	    val seq: t list -> t
	    val str: string -> t
	    val tt: t -> t
	    val table: tableOption list * t list list -> t
	 end
      structure Option:
	 sig
	    datatype t =
	       Redirect of {seconds: int,
			    uri: Uri.t}
	     | Title of string
	 end

      datatype t = T of {options: Option.t list,
			 body: Element.t}
      val layout: t -> Layout.t
   end
