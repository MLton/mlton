signature BASIS_NONE =
   sig
      (* Top-level types *)
      eqtype 'a array
      datatype bool = datatype BasisExtra.bool
      eqtype char
      type exn
      eqtype int 
      datatype list = datatype BasisExtra.list
      datatype ref = datatype BasisExtra.ref
      type real
      eqtype string
      type substring 
      eqtype unit
      eqtype 'a vector
      eqtype word 

      exception Bind
      exception Match
      exception Overflow

      val = : ''a * ''a -> bool
      val <> : ''a * ''a -> bool
   end
