fun F.f () = ()

fun f = ()

fun f _ = ()
  | f _ _ = ()

fun f _ = ()
  | g _ = ()
  | h _ = ()

fun f (): bool = 13

fun f (_: bool) = ()
  | f (_: int) = ()

fun f (): int = 13
  | f (): bool = true

fun f (x: int) = f true

val 'a x: 'a -> 'a = (fn y => y) (fn z => z)

val rec (x: int) = fn z => z

val x: int = true
