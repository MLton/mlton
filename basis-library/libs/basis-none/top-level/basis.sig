signature BASIS_NONE =
   sig
      (* Top-level types *)
      eqtype unit
      eqtype int 
      eqtype word 
      type real
      eqtype char
      eqtype string
      type substring 
      type exn
      eqtype 'a array 
      eqtype 'a vector
(*
      eqtype 'a ref
*)
      datatype 'a ref = ref of 'a
      datatype bool = false | true
      datatype 'a list = nil | :: of ('a * 'a list)

      exception Bind
      exception Match
      exception Overflow
   end
   (* Top-level types *)
   where type unit = unit
   where type int = int
   where type word = word
   where type real = real
   where type char = char
   where type exn = exn
   where type 'a array = 'a array
   where type 'a vector = 'a vector
   where type 'a ref = 'a ref
   where type bool = bool
   where type 'a list = 'a list