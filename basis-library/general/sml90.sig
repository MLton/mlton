signature SML90 =
   sig
      type instream
      type outstream
      exception Abs
      exception Quot
      exception Prod
      exception Neg
      exception Sum
      exception Diff
      exception Floor
      exception Exp
      exception Sqrt
      exception Ln
      exception Ord
      exception Mod
      exception Io of string 
      exception Interrupt
      val sqrt: real -> real 
      val exp: real -> real 
      val ln: real -> real 
      val sin: real -> real 
      val cos: real -> real 
      val arctan: real -> real 
      val ord: string -> int 
      val chr: int -> string 
      val explode: string -> string list 
      val implode: string list -> string 
      val std_in: instream 
      val open_in: string -> instream 
      val input: instream * int -> string 
      val lookahead: instream -> string
      val close_in: instream -> unit 
      val end_of_stream: instream -> bool 
      val std_out: outstream 
      val open_out: string -> outstream 
      val output: outstream * string -> unit 
      val close_out: outstream -> unit
   end
