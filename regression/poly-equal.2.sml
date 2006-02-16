fun assert (msg,b) =
   if b then ()
   else print ("assertion failed: " ^ msg ^ "\n")

datatype 'a tree =
   Leaf of 'a
  | Node of 'a * 'a tree * 'a tree
      
val _ =
   (assert ("string equal", "foobar" = "foobar") ;
    assert ("string not equal", "foobar" <> "foobaz") ;
    assert ("tuple equal", (1,2,3) = (1,2,3)) ;
    assert ("tuple not equal", (1,2,3) <> (1,2,4)) ;
    assert ("list equal", [1,2,3] = [1,2,3]) ;
    assert ("list not equal", [1,2,3] <> [1,2,3,4]) ;
    assert ("pair list equal", [(1,2), (3,4)] = [(1,2), (3,4)]) ;
    assert ("pair list not equal", [(1,2), (3,4)] <> [(1,2), (3,5)]) ;
    assert ("tree equal",
           let val t = Node (1, Leaf 2, Node (3, Leaf 4, Leaf 5))
           in t = t
           end))

    

