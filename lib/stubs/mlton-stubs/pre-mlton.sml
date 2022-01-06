(* Copyright (C) 2022 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PreMLton =
struct

   open MLton

   structure Platform =
      struct
         structure Arch =
            struct
               val host =
                  MLton.Platform.Arch.toString MLton.Platform.Arch.host
            end
         structure Format =
            struct
               val host =
                  MLton.Platform.Format.toString MLton.Platform.Format.host
            end
         structure OS =
            struct
               val host =
                  MLton.Platform.OS.toString MLton.Platform.OS.host
            end
      end

end
