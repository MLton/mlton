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
      datatype ref = datatype ref
      datatype bool = datatype bool
      datatype list = datatype list

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
