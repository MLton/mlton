
datatype ilist = nill | cons of int * ilist

fun f Xs =
 case Xs of
   nill => nill
 | cons( z, zs ) =>
 let
   fun g( a, b, c, ds ) =
     case ds of
       nill => cons( c, nill )
     | cons( e, es ) =>
     case ( e < b ) of
       false => cons( c, ds )
     | true =>
         cons(
           e,
           case ( z < a ) of
             false =>
               g( b, c, c, es )
           | true => cons( c, nill )
           )
 in
   case zs of
     nill => Xs
   | cons( y, ys ) =>
       g(
         z,
         z,
         z,
         f( g( z, z, y, ys ) )
         )
 end

fun print_ilist( nill ) = print "\n"
  | print_ilist( cons( X, Xs ) ) = ( print (Int.toString X ^ " "); print_ilist Xs )

val Input = cons( 6, cons( 5, cons( 7, cons( 3, cons( 1, cons( 2, cons( 4, nill ) ) ) ) ) ) )

val _ = print_ilist( f Input )
