signature ERROR =
    sig
       val bug: string -> 'a
       val unimplemented: string -> 'a
       val warning: string -> unit
   end
