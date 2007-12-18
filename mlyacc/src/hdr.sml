(* Modified by mfluet@acm.org on 2005-8-01.
 * Update with SML/NJ 110.55+.
 *)
(* Modified by sweeks@acm.org on 2000-8-24.
 * Ported to MLton.
 *)
type int = Int.int
   
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor HeaderFun () : HEADER =
  struct
        val DEBUG = true

        type pos = {line : int, col : int}
        val pos = {line = ref 1, start = ref 0}
        val text = ref (nil: string list)
        type inputSource = {name : string,
                            errStream : TextIO.outstream,
                            inStream : TextIO.instream,
                            errorOccurred : bool ref}

        val newSource = 
          fn (s : string,i : TextIO.instream ,errs : TextIO.outstream) =>
              {name=s,errStream=errs,inStream=i,
               errorOccurred = ref false}
                        
        val errorOccurred = fn (s : inputSource) =>fn () => !(#errorOccurred s)

        val pr = fn out : TextIO.outstream => fn s : string => TextIO.output(out,s)

        val error = fn {name,errStream, errorOccurred,...} : inputSource =>
              let val pr = pr errStream
              in fn l : pos => fn msg : string =>
                  (pr name; pr ", line "; pr (Int.toString (#line l));
                   pr ": Error: "; pr msg; pr "\n"; errorOccurred := true)
              end

        val warn = fn {name,errStream, errorOccurred,...} : inputSource =>
              let val pr = pr errStream
              in fn l : pos => fn msg : string =>
                  (pr name; pr ", line "; pr (Int.toString (#line l));
                   pr ": Warning: "; pr msg; pr "\n")
              end

        datatype prec = LEFT | RIGHT | NONASSOC

        datatype symbol = SYMBOL of string * pos
        val symbolName = fn SYMBOL(s,_) => s
        val symbolPos = fn SYMBOL(_,p) => p
        val symbolMake = fn sp => SYMBOL sp
    
        type ty = string
        val tyName = fn i => i
        val tyMake = fn i => i
 
        datatype control = NODEFAULT | VERBOSE | PARSER_NAME of symbol |
                           FUNCTOR of string  | START_SYM of symbol |
                           NSHIFT of symbol list | POS of string | PURE |
                           PARSE_ARG of string * string |
                           TOKEN_SIG_INFO of string
                           
        datatype declData = DECL of
                        {eop : symbol list,
                         keyword : symbol list,
                         nonterm : (symbol*ty option) list option,
                         prec : (prec * (symbol list)) list,
                         change: (symbol list * symbol list) list,
                         term : (symbol* ty option) list option,
                         control : control list,
                         value : (symbol * string) list}

        type rhsData = {rhs:symbol list,code:string, prec:symbol option} list
        datatype rule = RULE of {lhs : symbol, rhs : symbol list,
                                 code : {text : string, pos : pos},
                                 prec : symbol option}

        type parseResult = string * declData * rule list
        val getResult = fn p => p

        fun join_decls
              (DECL {eop=e,control=c,keyword=k,nonterm=n,prec,
                change=su,term=t,value=v}:declData,
               DECL {eop=e',control=c',keyword=k',nonterm=n',prec=prec',
                     change=su',term=t',value=v'} : declData,
               inputSource,pos) =
          let val ignore = fn s => 
                        (warn inputSource pos ("ignoring duplicate " ^ s ^
                                            " declaration"))
              val join = fn (e,NONE,NONE) => NONE
                          | (e,NONE,a) => a
                          | (e,a,NONE) => a
                          | (e,a,b) => (ignore e; a)
              fun mergeControl (nil,a) = [a]
                | mergeControl (l as h::t,a) =
                     case (h,a)
                     of (PARSER_NAME _,PARSER_NAME n1) => (ignore "%name"; l)
                      | (FUNCTOR _,FUNCTOR _) => (ignore "%header"; l)
                      | (PARSE_ARG _,PARSE_ARG _) => (ignore "%arg"; l)
                      | (START_SYM _,START_SYM s) => (ignore "%start"; l)
                      | (POS _,POS _) => (ignore "%pos"; l)
                      | (TOKEN_SIG_INFO _, TOKEN_SIG_INFO _)
                         => (ignore "%token_sig_info"; l)
                      | (NSHIFT a,NSHIFT b) => (NSHIFT (a@b)::t)
                      | _ => h :: mergeControl(t,a)
              fun loop (nil,r) = r
                | loop (h::t,r) = loop(t,mergeControl(r,h))
         in DECL {eop=e@e',control=loop(c',c),keyword=k'@k,
            nonterm=join("%nonterm",n,n'), prec=prec@prec',
            change=su@su', term=join("%term",t,t'),value=v@v'} :
                   declData
        end
end;

structure Header = HeaderFun();
      
