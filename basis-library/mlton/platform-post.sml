(* Copyright (C) 2022 Matthew Fluet
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Primitive =
struct

open Primitive

structure MLton =
   struct
      open MLton
      structure Platform =
         struct
            open MLtonPlatform
            structure Arch =
               struct
                  open MLton.Platform.Arch
                  open Arch
               end
            structure OS =
               struct
                  open OS
                  val forkIsEnabled =
                     case host of
                        Cygwin => Primitive.MLton.Platform.OS.cygwinUseMmap ()
                      | MinGW => false
                      | _ => true

                  val useWindowsProcess = not forkIsEnabled
               end
         end
   end

end
