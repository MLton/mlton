type int = Int.t
   
signature JUSTIFY =
   sig
      datatype t =
	 Left
       | Center
       | Right

      val justify: string * int * t -> string
      val outputTable: string list list * Out.t -> unit
      val table: {justs: t list,
		  rows: string list list} -> string list list
   end
