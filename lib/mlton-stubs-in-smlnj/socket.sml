structure Socket =
   let
      structure S = Socket
      open OpenInt32
   in
      struct
         open Socket

         structure Ctl =
            struct
               open Ctl

               val getNREAD = fn z => (fromInt o getNREAD) z

               val getRCVBUF = fn z => (fromInt o getRCVBUF) z

               val getSNDBUF = fn z => (fromInt o getSNDBUF) z

               val setRCVBUF =
                  fn z => (setRCVBUF o (fn (s, i) => (s, toInt i))) z

               val setSNDBUF =
                  fn z => (setSNDBUF o (fn (s, i) => (s, toInt i))) z
            end

         val listen = fn z => (listen o (fn (s, i) => (s, toInt i))) z

         val recvArr = fn z => (fromInt o recvArr) z

         val recvArr' = fn z => (fromInt o recvArr') z

         val recvArrFrom =
            fn z => ((fn (i, a) => (fromInt i, a)) o recvArrFrom) z

         val recvArrFrom' =
            fn z => ((fn (i, a) => (fromInt i, a)) o recvArrFrom') z

         val recvArrFromNB =
            fn z => ((fn NONE => NONE | SOME (i, a) => SOME (fromInt i, a))
                     o recvArrFromNB) z

         val recvArrFromNB' =
            fn z => ((fn NONE => NONE | SOME (i, a) => SOME (fromInt i, a))
                     o recvArrFromNB') z

         val recvArrNB = fn z => (fromIntOpt o recvArrNB) z

         val recvArrNB' = fn z => (fromIntOpt o recvArrNB') z

         val recvVec = fn z => (recvVec o (fn (s, i) => (s, toInt i))) z

         val recvVec' = fn z => (recvVec' o (fn (s, i, f) => (s, toInt i, f))) z

         val recvVecFrom = fn z => (recvVecFrom o (fn (s, i) => (s, toInt i))) z

         val recvVecFrom' =
            fn z => (recvVecFrom' o (fn (s, i, f) => (s, toInt i, f))) z

         val recvVecFromNB =
            fn z => (recvVecFromNB o (fn (s, i) => (s, toInt i))) z

         val recvVecFromNB' =
            fn z => (recvVecFromNB' o (fn (s, i, f) => (s, toInt i, f))) z

         val recvVecNB = fn z => (recvVecNB o (fn (s, i) => (s, toInt i))) z

         val sendArr = fn z => (fromInt o sendArr) z

         val sendArr' = fn z => (fromInt o sendArr') z

         val sendArrNB = fn z => (fromIntOpt o sendArrNB) z

         val sendArrNB' = fn z => (fromIntOpt o sendArrNB') z

         val sendVec = fn z => (fromInt o sendVec) z

         val sendVec' = fn z => (fromInt o sendVec') z

         val sendVecNB = fn z => (fromIntOpt o sendVecNB) z

         val sendVecNB' = fn z => (fromIntOpt o sendVecNB') z
      end
   end
