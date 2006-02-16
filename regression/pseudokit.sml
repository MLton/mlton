structure Set = struct datatype Set = S end

signature FINMAPEQ = sig type map val dom : map -> Set.Set end

functor FinMapEq() = struct datatype map = M fun dom m = Set.S end

signature TOOLS =
  sig
    structure FinMapEq : FINMAPEQ
  end

functor Tools (): TOOLS = struct structure FinMapEq = FinMapEq() end

(*
functor Basics(structure Tools : sig
                                   structure FinMapEq : sig type map val dom : map -> Set.Set end
                                 end) =
*)

functor Basics(structure Tools : TOOLS) =
  struct
    structure Tools = Tools
  end

functor KitCompiler() = 
  struct
    structure Tools = Tools()
    structure Basics = Basics(structure Tools = Tools)
  end

structure K = KitCompiler()

