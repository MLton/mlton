structure BasisNone: BASIS_NONE =
   struct
      open ArrayGlobal
	   BoolGlobal
	   CharGlobal
	   IntGlobal
	   GeneralGlobal
	   ListGlobal
	   RealGlobal
	   StringGlobal
	   RealGlobal
	   SubstringGlobal
	   VectorGlobal
	   WordGlobal

      val op = = op =
      val op <> = op <>
      datatype ref = datatype ref
   end
