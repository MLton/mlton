
  fun token0(tokFn) = tokFn

  and token1(tokFn, value) = tokFn(value)

  val a = token1(fn _ => "1", 2)
