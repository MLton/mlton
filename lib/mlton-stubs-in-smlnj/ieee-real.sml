structure IEEEReal =
   struct
      open IEEEReal

      datatype float_class =
	 NAN
       | INF
       | ZERO
       | NORMAL
       | SUBNORMAL
   end
