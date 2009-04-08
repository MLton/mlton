(* Modified by Vesa Karvonen on 2007-12-18.
 * Create line directives in output.
 *)
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

signature ABSYN =
    sig
       datatype exp = EVAR of string
                    | EAPP of exp * exp
                    | ETUPLE of exp list
                    | EINT of int
                    | FN of pat * exp
                    | LET of decl list * exp
                    | UNIT
                    | SEQ of exp * exp
                    | CODE of {text : string, pos : Header.pos}
       and      pat = PVAR of string
                    | PAPP of string * pat
                    | PTUPLE of pat list
                    | PLIST of pat list * pat option
                    | PINT of int
                    | WILD
                    | AS of string * pat
       and     decl = VB of pat * exp
       and     rule = RULE of pat * exp
       val printRule : ((string -> unit) * (string -> unit)
                        * (Header.pos option -> string))
                       -> rule -> unit
    end
