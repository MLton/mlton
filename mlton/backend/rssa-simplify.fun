(* Copyright (C) 2019-2021 Matthew Fluet.
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
structure InsertChecks = InsertChecks (S)

val rssaPasses =
   {name = "rssaShrink1", doit = S.shrink, execute = true} ::
   {name = "collectStatics.WordXVectorConsts",
    doit = CollectStatics.WordXVectorConsts.transform,
    execute = true} ::
   {name = "collectStatics.Globals",
    doit = CollectStatics.Globals.transform,
    execute = true} ::
   {name = "collectStatics.RealConsts",
    doit = CollectStatics.RealConsts.transform,
    execute = true} ::
   {name = "insertChecks", doit = InsertChecks.transform, execute = true} ::
   {name = "rssaShrink2", doit = S.shrink, execute = true} ::
   (* must be before implementHandlers *)
   {name = "bounceVars", doit = BounceVars.transform, execute = true} ::
   {name = "implementHandlers", doit = ImplementHandlers.transform, execute = true} ::
   {name = "rssaShrink3", doit = S.shrink, execute = true} ::
   {name = "implementProfiling", doit = ImplementProfiling.transform, execute = true} ::
   {name = "rssaOrderFunctions", doit = Program.orderFunctions, execute = true} ::
   {name = "rssaShuffle", doit = Program.shuffle, execute = false} ::
   nil

fun simplify p =
   let
      val rssaPasses = rssaPasses
      val p =
         Control.simplifyPasses
         {arg = p,
          passes = rssaPasses,
          stats = Program.layoutStats,
          toFile = Program.toFile,
          typeCheck = typeCheck}
   in
      p
   end
end
