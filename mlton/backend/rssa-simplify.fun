(* Copyright (C) 2019-2020 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor RssaSimplify (S: RSSA_SIMPLIFY_STRUCTS): RSSA_SIMPLIFY =
struct

open S

structure BounceVars = BounceVars (S)
structure CollectStatics = CollectStatics (S)
structure ImplementHandlers = ImplementHandlers (S)
structure ImplementProfiling = ImplementProfiling (S)
structure LimitCheck = LimitCheck (S)
structure SignalCheck = SignalCheck(S)

val rssaPasses =
   {name = "rssaShrink1", doit = S.shrink, execute = true} ::
   {name = "collectStatics.WordXVectorConsts",
    doit = CollectStatics.WordXVectorConsts.transform,
    execute = false} ::
   {name = "collectStatics.Globals",
    doit = CollectStatics.Globals.transform,
    execute = false} ::
   {name = "collectStatics.RealConsts",
    doit = CollectStatics.RealConsts.transform,
    execute = false} ::
   {name = "insertLimitChecks", doit = LimitCheck.transform, execute = true} ::
   {name = "insertSignalChecks", doit = SignalCheck.transform, execute = true} ::
   (* must be before implementHandlers *)
   {name = "bounceVars", doit = BounceVars.transform, execute = true} ::
   {name = "implementHandlers", doit = ImplementHandlers.transform, execute = true} ::
   {name = "rssaShrink2", doit = S.shrink, execute = true} ::
   {name = "implementProfiling", doit = ImplementProfiling.transform, execute = true} ::
   {name = "rssaOrderFunctions", doit = Program.orderFunctions, execute = true} ::
   {name = "rssaShuffle", doit = Program.shuffle, execute = false} ::
   nil

fun simplify p =
   let
      val rssaPasses = rssaPasses
      (* RSSA type check is too slow to run by default. *)
      (* val () = Control.trace (Control.Pass, "rssaTypeCheck") typeCheck p *)
      val p =
         Control.simplifyPasses
         {arg = p,
          passes = rssaPasses,
          stats = Program.layoutStats,
          toFile = Program.toFile,
          typeCheck = typeCheck}
      (* val () = Control.trace (Control.Pass, "rssaTypeCheck") typeCheck p *)
   in
      p
   end
end
