type int = Int.t
   
signature ENGINE =
   sig
      type 'a t

      datatype 'a res =
	 Done of 'a
       | Raise of exn
       | TimeOut of 'a t

      val new: (unit -> 'a) -> 'a t
      val repeat: {thunk: unit -> 'a,
		   limit: Time.t,
		   tries: int} -> 'a option
      val run: 'a t * Time.t -> 'a res
      val timeLimit: Time.t * (unit -> 'a) -> 'a option
   end
