signature ABSYN =
  sig
     type absyn
     val null : absyn
  end

abstraction Absyn : ABSYN =
   struct
      datatype absyn = NULL
      val null = NULL
   end