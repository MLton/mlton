(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor ExtractTargs(S: EXTRACT_TARGS_STRUCTS): EXTRACT_TARGS = 
struct

open S


(* val extract =
 *    Trace.trace("extract",
 * 	       fn {prim, args, result} =>
 * 	       let open Layout
 * 	       in record[("prim", Prim.layout prim),
 * 			 ("args", List.layout Type.layout args),
 * 			 ("result", Type.layout result)]
 * 	       end,
 * 	       List.layout Type.layout)
 *    extract
 *)
   
end
