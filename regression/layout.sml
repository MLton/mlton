
fun layout (cbs : (string * real) list list) : string =
  let 
    val layoutcb =
      map (fn (con,_) => con)

    fun layoutdb cb = "{" ^ concat(layoutcb cb) ^ "}"
  in concat(map layoutdb cbs)
  end
