datatype b = TRUE | FALSE
   
datatype nat = Z | S of nat
   
fun lt (Z, S _) = TRUE
  | lt (S n1, S n2) = lt (n1, n2)
  | lt _ = FALSE
   
fun ZZZ_f (a, b) = lt (#value a, #value b)
   
fun ZZZ_copyTo (array, record as {value, offset}) =
   fn S Z => {value = value, offset = S (S (S Z))}
    | n => array n

fun ZZZ_fixTooBig (array, record) =
   let
      val left = array (S Z)
      val right = array (S (S Z))
      val small = 
         case ZZZ_f (left, right) of 
            TRUE => left 
          | FALSE => right
   in
      case ZZZ_f (record, small) of
         TRUE => ZZZ_copyTo (array, record)
       | FALSE => ZZZ_copyTo (array, small)
   end
