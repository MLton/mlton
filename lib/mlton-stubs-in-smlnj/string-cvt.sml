structure StringCvt =
   struct
      open StringCvt

      open OpenInt32
      (* the val rec's are just to remove the constructor status *)
      val rec SCI = fn x => x
      val rec FIX = fn x => x
      val rec GEN = fn x => x
      val SCI = StringCvt.SCI o toIntOpt
      val FIX = StringCvt.FIX o toIntOpt
      val GEN = StringCvt.GEN o toIntOpt

      fun padLeft c i = StringCvt.padLeft c (toInt i)
      fun padRight c i = StringCvt.padRight c (toInt i)
   end
