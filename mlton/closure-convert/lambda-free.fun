(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor LambdaFree (S: LAMBDA_FREE_STRUCTS): LAMBDA_FREE = 
struct

open S
open Dec PrimExp

structure Status =
   struct
      datatype t = Unseen | Free | Bound

      val init = Unseen
   end
datatype status = datatype Status.t

fun lambdaFree {program = Program.T {body, ...},
                overflow: Var.t,
                varInfo: Var.t -> {frees: Var.t list ref ref,
                                   status: Status.t ref},
                lambdaInfo: Lambda.t -> {frees: Var.t vector ref,
                                         recs: Var.t vector ref}} =
   let
      fun setFree (l: Lambda.t, xs: Var.t vector): unit =
         #frees (lambdaInfo l) := xs
      fun setRec (l: Lambda.t, xs: Var.t vector): unit =
         #recs (lambdaInfo l) := xs
      type scope = {frees: Var.t list ref,
                    get: Var.t -> Status.t,
                    set: Var.t * Status.t -> unit}
      fun bind (x: Var.t, {set, ...}: scope) = set (x, Bound)
      fun var (x: Var.t, {get, set, frees}: scope) =
         case get x of
            Unseen => (set (x, Free); List.push (frees, x))
          | _ => ()
      fun vars (xs, s) = Vector.foreach (xs, fn x => var (x, s))
      fun varExp (x: VarExp.t, s: scope) = var (VarExp.var x, s)
      fun varExpOpt (x, s) =
         case x of
            NONE => ()
          | SOME x => varExp (x, s)
      fun varExps (xs, s) = Vector.foreach (xs, fn x => varExp (x, s))

      (*
        newScope is invoked whenever there is a need to consider a new scope while
        looking for free variables. Its only parameter is a function taking a record that
        represents a scope supporting "setting" and "getting" variable statuses.
        The intent is that `th` will continue traversing the program in the current
        scope while aggregating variable statuses.

        Initially, newScope creates a reference to a list of variables (`frees`)
        Its purpose is twofold:
            - It is a unique identifier for every encountered scope.
            - It is utilized by `th` to aggregate all variabes

        Since each variable has an associated status, updating every single status
        in the program would be unreasonably slow. Thus, we delay updating the status
        by associating each variable with the last scope for which that variable was
        seen. If the variable has been unmentioned until this point in the current scope,
        then we save its last scope and status, and "initialize" it to be Unseen.
        This is achieved by having `get` and `set` use the `statusRef` function.

        After setting up these operations, we perform `th`, and then recover
        every variable's previous status and scope so that we may continue
        traversing the program.
       *)
      fun newScope
            (th: {frees: Var.t list ref,
                  get:   Var.t -> Status.t,
                  set:   Var.t * Status.t -> unit } -> unit)
            : Var.t vector =
         let
            val frees = ref []
            val all = ref []
            fun statusRef x =
               let val {frees = frees', status, ...} = varInfo x
               in if frees = !frees'
                     then ()
                  else (List.push (all, (frees', !frees', status, !status))
                        ; frees' := frees; status := Unseen)
                     ; status
               end
            fun get x = !(statusRef x)
            fun set (x, s) = statusRef x := s
            val _ = th {frees = frees, get = get, set = set}
            val _ = List.foreach (!all, fn (r, v, r', v') => (r := v; r' := v'))
         in
            Vector.fromList (!frees)
         end

      fun exp (e, s) =
         let val {decs, result} = Exp.dest e
         in List.foreach
            (decs,
             fn Exception _ => ()
              | MonoVal {var, exp, ...} => (primExp (exp, s); bind (var, s))
              | PolyVal {var, exp = e, ...} => (exp (e, s); bind (var, s))
              | Fun {decs, ...} =>
                   let
                      val {get = isBound, set, destroy} =
                         Property.destGetSetOnce (Var.plist,
                                                  Property.initConst false)

                      (* Consider each of the functions in this function group to be bound
                         according to a property list. *)
                      val _ = Vector.foreach (decs, fn {var, ...} => set (var, true))

                      (* Consider this recursive function group to be part of a new scope.
                         Then accumulate all free variables from each function (`lambda l`)
                         and if it is a mutually recursive function from this group (i.e. it
                         was marked as bound), then treat it as such; otherwise, delegate
                         the responsibility of checking/setting the variable to the var function *)
                      val xs =
                         newScope
                         (fn s =>
                          Vector.foreach
                          (decs, fn {lambda = l, ...} =>
                           setRec (l,
                                   Vector.keepAll
                                   (lambda l, fn x =>
                                    if isBound x
                                       then true
                                    else (var (x, s); false)))))
                       
                      (* Get rid of the list of mutually recursive functions *)
                      val _ = destroy ()

                      (* Each function in this function group will have the same associated free variables.
                         Its name will then be bound to the current scope. *)
                      val _ =
                         Vector.foreach (decs, fn {var, lambda, ...} =>
                                         (setFree (lambda, xs)
                                          ; bind (var, s)))
                   in
                      vars (xs, s)
                   end)
            ; varExp (result, s)
         end
      and primExp (e, s) = 
         case e of
            App {func, arg} => (varExp (func, s); varExp (arg, s))
          | Case {test, cases, default} =>
               (varExp (test, s)
                ; Option.app (default, fn (e, _) => exp (e, s))
                ; Cases.foreach' (cases, fn e => exp (e, s),
                                  fn Pat.T {arg, ...} =>
                                  Option.app (arg, fn (x, _) => bind (x, s))))
          | ConApp {arg, ...} => varExpOpt (arg, s)
          | Const _ => ()
          | Handle {try, catch, handler} =>
               (exp (try, s); bind (#1 catch, s); exp (handler, s))
          | Lambda l =>
               let val xs = lambda l
               in setFree (l, xs); vars (xs, s)
               end
          | PrimApp {prim, args, ...} => 
               (if Prim.mayOverflow prim
                  then var (overflow, s)
                  else ();
                varExps (args, s))
          | Profile _ => ()
          | Raise {exn, ...} => varExp (exn, s)
          | Select {tuple, ...} => varExp (tuple, s)
          | Tuple xs => varExps (xs, s)
          | Var x => varExp (x, s)
      and lambda (l: Lambda.t) : Var.t vector =
         let val {arg, body, ...} = Lambda.dest l
         in newScope (fn s => (bind (arg, s); exp (body, s)))
         end
      val frees = newScope (fn s => exp (body, s))
      val _ =
         if Vector.isEmpty frees
            then ()
         else Error.bug ("LambdaFree.lambdaFree: program has free variables: " ^
                         (Layout.toString (Vector.layout Var.layout frees)))
   in
      ()
   end

end
