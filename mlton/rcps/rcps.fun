
functor Rcps(S: RCPS_STRUCTS): RCPS =
  Simplify(TypeCheck(RcpsTree(S)))