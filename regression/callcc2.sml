type ident = string
type con = string

datatype pattern = 
    PVar of ident
  | PAlias of ident * pattern
  | PConstruct of con * pattern list
  | PAliasD of ident * pattern
  | PConstructD of con * pattern list

datatype exp = 
    Var of ident
  | Lam of ident * exp
  | App of exp * exp
  | Construct of con * exp list
  | Case of exp * (pattern * exp) list
  | Let of ident * exp * exp
 
  | LamD of ident * exp
  | AppD of exp * exp
  | ConstructD of con * exp list
  | CaseD of exp * (pattern * exp) list
  | LetD of ident * exp * exp

  | Lift of exp

datatype value =
    Fun of (value -> value)
  | Con of con * value list
  | Code of exp
  | Wrong

val valueToString =
   fn Fun _ => "Fun"
    | Con _ => "Con"
    | Code _ => "Code"
    | Wrong => "Wrong"

(* control operators *)
(*********************)

(* toplevel resetMarker *)
val metaCont = ref (fn (x : value) => x)

fun abort thunk = 
    let val v = thunk () in
        !metaCont v
    end

fun reset thunk = 
    let val mc = !metaCont in
        SMLofNJ.Cont.callcc 
        (fn k => let (* new marker which restores old one *)
                     val _ = metaCont := (fn v => 
                                          let val _ = metaCont := mc in
                                              SMLofNJ.Cont.throw k v
                                          end)
                 in
                     abort thunk
                 end)
    end

fun shift f =
    SMLofNJ.Cont.callcc
    (fn k => abort (fn () => f 
                    (fn v => reset 
                     (fn () => SMLofNJ.Cont.throw k v))))

(*********************)

(* environment *)
exception UnboundVar of ident

fun update r var value = (var, value) :: r
        
fun lookup [] var = raise (UnboundVar var)
  | lookup ((var, value) :: r) var' =
    if var = var' then value else lookup r var'

(* pattern matcher - binds variables 
   patterns are linear and pairwise disjoint *)
fun patterneq (p, value) r =
    case p of
        PVar x => (update r x value, true)
      | PAlias (x, p) => 
            let val (r', eq) = patterneq (p, value) r in
                (update r' x value, eq)
            end
      | PConstruct (c, ps) =>
            let val Con(c', vs) = value 
                val eq = (c = c')
                val eq = eq andalso (List.length vs = List.length ps) 
            in
                List.foldl (fn ((p, v), (r', eq')) =>
                            let val (r'', eq'') = patterneq (p, v) r' in
                                (r'', eq'' andalso eq')
                            end) (r, eq) (ListPair.zip (ps, vs))
            end

val gensym = 
    let val count = ref 0 in
        (fn x => (count := !count + 1;
                  (x^(Int.toString (!count)))))
    end

(* copies pattern with fresh variables bound in new environment *)
fun generatePattern (r, p) =
    case p of
        PVar x => 
            let val xx = gensym x in 
                (update r x (Code (Var xx)), PVar xx) 
            end
      | PAliasD (x, p) => 
            let val (r', p') = generatePattern (r, p) 
                val xx = gensym x 
            in
                (update r x (Code (Var xx)),
                 PAlias (xx, p'))
            end
      | PConstructD (c, ps) =>
            let val (r, ps) = 
                List.foldr (fn (p, (r, ps)) => 
                            let val (r', p') = generatePattern (r, p) in
                                (r', p' :: ps)
                            end) (r, []) ps
            in
                (r, PConstruct (c, ps))
            end
        
(* the specializer *)
fun spec e r =
    case e of
        Var x => lookup r x
            
      (* Specialization of Static Stuff - standard semantics *)
      | Lam (x, e) => Fun (fn y => spec e (update r x y))
            
      | App (f, a) => 
            let val Fun ff = spec f r in 
                ff (spec a r) 
            end 
        
      | Construct (c, es) =>
            let val vs = List.map (fn e => spec e r) es in
                Con (c, vs)
            end
        
      | Case (test, cls) =>
            let val testv = spec test r 
                (* exhaustive by restriction on patterns *)
                fun loop cls =
                    (case cls of
                         ((p, e) :: cls) =>
                             let val (r', eq) = patterneq (p, testv) r in
                                 if eq then spec e r' else loop cls
                             end
                       | [] => Wrong)
            in loop cls end
        
      | Let (x, e1, e2) => let val v1 = spec e1 r in spec e2 (update r x v1) end
  
      (* Specialization of Dynamic stuff *)
      | LamD (x, e) => 
                let val xx = gensym x  
                    val Code body = 
                        reset (fn () => spec e (update r x (Code (Var xx)))) 
                in
                    Code (Lam (xx, body))
                end
            
      | AppD (f, a) => 
                let val Code ff = spec f r 
                    val Code aa = spec a r 
                in
                    Code (App (ff, aa))
                end
            
      | ConstructD (c, es) =>
                let val es' = List.map (fn e => let val Code v = spec e r
                                                in v end) es
                in
                    Code (Construct (c, es'))
                end
            
      | LetD (x, e1, e2) => 
                let val xx = gensym x in
                    shift (fn k => 
                           let val Code e1' = spec e1 r 
                               val Code e2' = 
                                   reset (fn () => k (spec e2 (update r x (Code (Var xx))))) 
                           in
                               Code (Let (xx, e1', e2'))
                           end)
                end
            
      | CaseD (test, cls) =>
                shift (fn k =>
                       let val Code testd = spec test r  
                           val newCls = List.map (fn (p, e) =>
                                                  let val (r', p') = generatePattern(r, p) 
                                                      val Code branch = reset (fn () => k (spec e r')) 
                                                  in
                                                      (p', branch)
                                                  end) cls
                       in
                           Code (Case(testd, newCls))
                       end)
                
      (* first-order lifting *)
      | Lift e => 
                let val Con(c, []) = spec e r in
                    Code(Construct (c, []))
                end

fun specialize p = spec p []

(* standard evaluation *)
val sampleProg1 = Lam("q", App(Let("id",
                                   App(Var "q", Var "q"),
                                   Lam("z", Var "z")),
                               Var "q"))

val sampleProg2 = Lam("f", App(Lam("x",
                                   Case(Var "x", 
                                        [(PConstruct("True",[]), 
                                          Lam("x",Lam("y",Var "x"))), 
                                         (PConstruct("False",[]), 
                                          Lam("x",Lam("y",Var "y")))])), 
                               Var "f"))

(* partial evaluation *)
val sampleProg1D = LamD("q", App(LetD("id",
                                      AppD(Var "q", Var "q"),
                                      Lam("z", Var "z")),
                                 Var "q"))
         
val sampleProg2D = LamD("f", LamD("x", 
                                  App(CaseD(Var "x", 
                                            [(PConstructD("True",[]), 
                                              Lam("z",LamD("y", Var "z"))), 
                                             (PConstructD("False",[]),  
                                              Lam("z",LamD("y", Var "y")))]), 
                                      Var "f")))

val specialize =
   fn p =>
   let val v = specialize p
   in print(valueToString v)
      ; print "\n"
   end

val v1 = specialize sampleProg1
val v2 = specialize sampleProg2
val v3 = specialize sampleProg1D  
val v4 = specialize sampleProg2
