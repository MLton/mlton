val l = [12345678901234567890,
         1234567890,
         1234,
         12,
         1,
         0,
         ~1,
         ~12,
         ~1234,
         ~1234567890,
         ~12345678901234567890]
         
val _ =
   List.app
   (fn i =>
    List.app
    (fn i' =>
     let
        val s =
           case IntInf.compare (i, i') of
              EQUAL => "equal"
            | GREATER => "greater"
            | LESS => "less"
     in
        print (concat [s, "\n"])
     end)
    l)
   l
