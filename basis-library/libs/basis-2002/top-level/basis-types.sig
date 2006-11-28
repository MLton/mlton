signature BASIS_2002_TYPES =
   sig
      (* Top-level types *)
      eqtype 'a array
      datatype bool = datatype bool
      eqtype char
      type exn
      eqtype int 
      datatype 'a option = NONE | SOME of 'a 
      datatype order = LESS | EQUAL | GREATER 
      datatype list = datatype list
      datatype ref = datatype ref
      type real
      eqtype string
      type substring 
      eqtype unit
      eqtype 'a vector
      eqtype word 
   end
