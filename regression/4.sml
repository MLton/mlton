(* This code has a handler which is provably unreachable. *)

fun f(n: int): int list =
   if n = 0
      then []
   else
      let val x = f(n - 1)
      in [13] handle e => (hd x + 1; raise e)
      end

val _ = f 13
