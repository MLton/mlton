val rec down =
   fn 0 => 0
    | n => 1 + down' (n - 1) + down (n - 1)
and down' =
   fn 0 => 0
    | n => 1 + down (n - 1) + down' (n - 1)

val _ = down 13
