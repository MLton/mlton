signature ABSYN =
  sig
     type absyn
     val null : absyn
  end

structure Absyn :> ABSYN =
   struct
      datatype absyn = NULL
      val null = NULL
   end