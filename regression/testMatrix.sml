
exception Error

fun translateXYZ (translateX : real, translateY : real, translateZ : real) =
      ((1.0, 0.0, 0.0, translateX),
       (0.0, 1.0, 0.0, translateY),
       (0.0, 0.0, 1.0, translateZ),
       (0.0, 0.0, 0.0, 1.0))

fun transformPoint matr {x, y, z} =
          let
            val ((in00, in01, in02, in03),
                 (in10, in11, in12, in13),
                 (in20, in21, in22, in23),
                 (in30, in31, in32, in33)) = matr
             
            val w = x * in30 + y * in31 + z * in32 + in33
          in
            if Real.== (0.0, w) then
              raise Error
            else
              {x = ((in00 * x) + (in01 * y) + (in02 * z) + in03) / w,
               y = ((in10 * x) + (in11 * y) + (in12 * z) + in13) / w,
               z = ((in20 * x) + (in21 * y) + (in22 * z) + in23) / w}
          end

fun Point3DToString {x, y, z} =
          "{x = " ^ (Real.toString x) ^
          ", y = " ^ (Real.toString y) ^
          ", z = " ^ (Real.toString z) ^
          "}\n"

val m = translateXYZ (2.0, 3.0, 4.0)
val p = {x = 1.0, y = 2.0, z = 3.0}
val _ = print (Point3DToString p)
val p' = transformPoint m p
val _ = print (Point3DToString p')
