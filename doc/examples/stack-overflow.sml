fun overflow() = (1 + overflow(); raise Bind)
val _ = overflow()
