signature GENERAL_GLOBAL =
  sig
     eqtype unit

     type exn
     exception Bind
     exception Match
     exception Chr
     exception Div
     exception Domain
     exception Fail of string 
     exception Overflow
     exception Size
     exception Span
     exception Subscript

     val exnName: exn -> string 
     val exnMessage: exn -> string

     datatype order = LESS | EQUAL | GREATER

     val ! : 'a ref -> 'a 
     val := : ('a ref * 'a) -> unit 
     val o : (('b -> 'c) * ('a -> 'b)) -> 'a -> 'c 
     val before: ('a * unit) -> 'a 
     val ignore: 'a -> unit
  end

signature GENERAL =
   sig
      include GENERAL_GLOBAL
   end

signature GENERAL_EXTRA =
   sig
      include GENERAL

      val addExnMessager: (exn -> string option) -> unit
   end
