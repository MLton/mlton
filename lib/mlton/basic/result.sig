signature RESULT =
   sig
      datatype 'a t =
	 No of string
       | Yes of 'a

      val isNo: 'a t -> bool
      val isYes: 'a t -> bool
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val map: 'a t * ('a -> 'b) -> 'b t
   end
      
