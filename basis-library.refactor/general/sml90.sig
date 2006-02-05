signature SML90 =
   sig
      type instream
      type outstream
      exception Abs
      exception Diff
      exception Exp
      exception Floor
      exception Interrupt
      exception Io of string 
      exception Ln
      exception Mod
      exception Neg
      exception Ord
      exception Prod
      exception Quot
      exception Sqrt
      exception Sum
      val arctan: real -> real 
      val chr: int -> string 
      val close_in: instream -> unit 
      val close_out: outstream -> unit
      val cos: real -> real 
      val end_of_stream: instream -> bool 
      val exp: real -> real 
      val explode: string -> string list 
      val implode: string list -> string 
      val input: instream * int -> string 
      val ln: real -> real 
      val lookahead: instream -> string
      val open_in: string -> instream 
      val open_out: string -> outstream 
      val ord: string -> int 
      val output: outstream * string -> unit 
      val sin: real -> real 
      val sqrt: real -> real 
      val std_in: instream 
      val std_out: outstream 
   end
