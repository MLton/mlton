/**
 * Name: sml_simple
 * Description: Standard ML programming language.
 * Author: Matthew Fluet <mfluet@acm.org>
 */

/*
builtin_face        --- 
comment_face        --- comments
function_name_face  --- 
highlight_face      --- 
keyword_face        --- keywords
reference_face      --- 
string_face         --- strings
type_face           --- 
variable_name_face  --- 
*/

sml_keywords_re =
/* Keywords:
(build-re '(; Core
            abstype  and  andalso  as  case  do  datatype  else
            end  exception  fn  fun  handle  if  in  infix
            infixr  let  local  nonfix  of  op  open  orelse
            raise  rec  then  type  val  with  withtype  while
            ; Modules
            eqtype  functor  include  sharing  sig
            signature  struct  structure  where)) ;'
*/
  /\b(a(bstype|nd(|also)|s)|case|d(atatype|o)|e(lse|nd|qtype|xception)|f(n|un(|ctor))|handle|i(f|n(|clude|fix(|r)))|l(et|ocal)|nonfix|o(f|p(|en)|relse)|r(aise|ec)|s(haring|ig(|nature)|truct(|ure))|t(hen|ype)|val|w(h(ere|ile)|ith(|type)))\b/;

state sml_simple
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /*
   * Keywords
   */
  sml_keywords_re {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  /*
   * Special constants (strings)
   */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (sml_string);
    string_face (false);
  }

  /*
   * Special constants (chars)
   */
  /(#)(\")/ {
    language_print ($1);
    string_face (true);
    language_print ($2);
    call (sml_string);
    string_face (false);
  }

  /*
   * Comments
   */
  /\(\*/ {
    comment_face (true);
    language_print ($0);
    call (sml_comment);
    comment_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

/*
 * Strings
 */
state sml_string
{
  /\\\\./ {
    language_print ($0);
  }

  /\"/ {
    language_print ($0);
    return;
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

/*
 * Nested comments
 */
state sml_comment
{
  BEGIN {
    sml_comment_depth = 1;
  }

  /\(\*/ {
    sml_comment_depth += 1;
    language_print ($0);
  }

  /\*\)/ {
    sml_comment_depth -= 1;
    language_print ($0);
    if (sml_comment_depth == 0)
      return;
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/*
Local variables:
mode: c
End:
*/

/**
 * Name: sml_verbose
 * Description: Standard ML programming language.
 * Author: Matthew Fluet <mfluet@acm.org>
 */

/*
builtin_face        --- 
comment_face        --- comments
function_name_face  --- 
highlight_face      --- 
keyword_face        --- keywords
reference_face      --- special constants (nums)
string_face         --- strings
type_face           --- 
variable_name_face  --- 
*/

/*
require_state (sml_simple);
*/

/*
formatting = [\ \t\n\011\012\013]+;
*/
sml_formatting_str = "[ \\t\\n\\011\\012\\013]+";
sml_formatting_re  = regexp(sml_formatting_str);
/*
letter     = [A-Za-z];
*/
sml_letter_str = "[A-Za-z]";
sml_letter_re  = regexp(sml_letter_str);
/*
symbol     = [-!%&$#+/:<=>?@\\~`|*^];
*/
sml_symbol_str = "[-!%&$#+/:<=>?@\\\\~`|*^]";
sml_symbol_re  = regexp(sml_symbol_str);
/*
digit = [0-9];
*/
sml_digit_str = "[0-9]";
sml_digit_re  = regexp(sml_digit_str);
/*
hexdigit   = [0-9a-fA-F];
*/
sml_hexdigit_str = "[0-9a-fA-F]";
sml_hexdigit_re  = regexp(sml_hexdigit_str);

/*
posdecint  = {digit}+;
*/
sml_posdecint_str = sprintf("(%s)+",sml_digit_str);
sml_posdecint_re  = regexp(sml_posdecint_str);
/*
poshexint  = "0x"{hexdigit}+;
*/
sml_poshexint_str = sprintf("0x(%s)+",sml_hexdigit_str);
sml_poshexint_re  = regexp(sml_poshexint_str);
/*
negdecint  = "~"{posdecint};
*/
sml_negdecint_str = sprintf("~(%s)",sml_posdecint_str);
sml_negdecint_re  = regexp(sml_negdecint_str);
/*
neghexint  = "~"{poshexint};
*/
sml_neghexint_str = sprintf("~(%s)",sml_poshexint_str);
sml_neghexint_re  = regexp(sml_neghexint_str);
/*
decint     = {posdecint} | {negdecint};
*/
sml_decint_str = sprintf("(%s)|(%s)",sml_posdecint_str,sml_negdecint_str);
sml_decint_re  = regexp(sml_decint_str);
/*
hexint     = {poshexint} | {neghexint};
*/
sml_hexint_str = sprintf("(%s)|(%s)",sml_poshexint_str,sml_negdecint_str);
sml_hexint_re  = regexp(sml_hexint_str);
/*
decword    = "0w"{digit}+;
*/
sml_decword_str = sprintf("0w(%s)+",sml_digit_str);
sml_decword_re  = regexp(sml_decword_str);
/*
hexword    = "0wx"{hexdigit}+;
*/
sml_hexword_str = sprintf("0wx(%s)+",sml_hexdigit_str);
sml_hexword_re  = regexp(sml_hexword_str);

/*
exp        = "E" | "e";
*/
sml_exp_str = "E|e";
sml_exp_re  = regexp(sml_exp_str);
/*
real       = ({decint}"."{digit}+ ({exp}{decint})?) | ({decint}{exp}{decint});
*/
sml_real_str = sprintf("((%s)\\.(%s)+((%s)(%s))?)|((%s)(%s)(%s))",
                       sml_decint_str,sml_digit_str,sml_exp_str,sml_decint_str,
                       sml_decint_str,sml_exp_str,sml_decint_str);
sml_real_re  = regexp(sml_real_str);

/*
scon_num = {decint} | {hexint} | {decword} | {hexword} | {real}
*/
sml_scon_num_str = sprintf("\\b((%s)|(%s)|(%s)|(%s)|(%s))\\b",
                           sml_decint_str,sml_hexint_str,
                           sml_decword_str,sml_hexword_str,
                           sml_real_str);
sml_scon_num_re = regexp(sml_scon_num_str);

/*
numericlab = [1-9]{digit}*;
*/
sml_numericlab_str = sprintf("[1-9](%s)*",sml_digit_str);
sml_numericlab_re  = regexp(sml_numericlab_str);
/*
alphanumid = {letter}({letter} | {digit} | [_'])*;
*/
sml_alphanumid_str = sprintf("(%s)((%s)|(%s)|[_'])*",
                             sml_letter_str,sml_letter_str,sml_digit_str);
/*
symbolicid = {symbol}+;
*/
sml_symbolicid_str = sprintf("(%s)+",sml_symbol_str);
sml_symbolicid_re  = regexp(sml_symbolicid_str);
/*
id         = {alphanumid} | {symbolicid};
*/
sml_id_str = sprintf("(%s)|(%s)",sml_alphanumid_str,sml_symbolicid_str);
sml_id_re  = regexp(sml_id_str);
/*
tyvar      = "'"({letter} | {digit} | [_'])*;
*/
sml_tyvar_str = sprintf("'((%s)|(%s)|[_'])*",sml_letter_str,sml_digit_str);
sml_tyvar_re  = regexp(sml_tyvar_str);
/*
longid     = ({alphanumid}".")+ {id};
*/
sml_longid_str = sprintf("((%s)\\.)+(%s)",sml_alphanumid_str,sml_id_str);
sml_longid_re  = regexp(sml_longid_str);

/*
lab    = {id} | {numericlab}
*/
sml_lab_str = sprintf("(%s)|(%s)",sml_id_str,sml_numericlab_str);
sml_lab_re  = regexp(sml_lab_str);
/*
sel    = "#" {formatting} {lab}
*/
sml_sel_str = sprintf("#(%s)(%s)",sml_formatting_str,sml_lab_str);
sml_sel_re  = regexp(sml_sel_str);
/*
infix  = "infix"("r")? {formatting} ({digit})?
*/
sml_infix_str = sprintf("(infix(r?))(%s)(%s)?",sml_formatting_str,sml_digit_str);
sml_infix_re  = regexp(sml_infix_str);

/*
val_with_tyvar = "val" {formatting} {tyvar}
val_with_tyvarseq = "val" {formatting} "(" {formatting} {tyvar}
fun_with_tyvar = "fun" {formatting} {tyvar}
fun_with_tyvarseq = "fun" {formatting} "(" {formatting} {tyvar}
*/
sml_val_with_tyvar_str = sprintf("(val)(%s)(%s)",sml_formatting_str,sml_tyvar_str);
sml_val_with_tyvar_re = regexp(sml_val_with_tyvar_str);
sml_val_with_tyvarseq_str = sprintf("(val)(%s)(\\\()(%s)(%s)",sml_formatting_str,sml_formatting_str,sml_tyvar_str);
sml_val_with_tyvarseq_re = regexp(sml_val_with_tyvarseq_str);
sml_fun_with_tyvar_str = sprintf("(fun)(%s)(%s)",sml_formatting_str,sml_tyvar_str);
sml_fun_with_tyvar_re = regexp(sml_fun_with_tyvar_str);
sml_fun_with_tyvarseq_str = sprintf("(fun)(%s)(\\\()(%s)(%s)",sml_formatting_str,sml_formatting_str,sml_tyvar_str);
sml_fun_with_tyvarseq_re = regexp(sml_fun_with_tyvarseq_str);


state sml_verbose /* extends sml_simple */
{
  sml_infix_re {
    keyword_face (true);
    language_print ($1);
    keyword_face (false);
    language_print ($3);
    language_print ($4);
  }
  sml_sel_re {
    language_print ($0);
  }

  /*
   * Special constants (nums)
   */
  sml_scon_num_re {
    reference_face (true);
    language_print ($0);
    reference_face (false);
  }

  /* extend sml_simple */
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /*
   * Keywords
   */
  sml_keywords_re {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  /*
   * Special constants (strings)
   */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (sml_string);
    string_face (false);
  }

  /*
   * Special constants (chars)
   */
  /(#)(\")/ {
    language_print ($1);
    string_face (true);
    language_print ($2);
    call (sml_string);
    string_face (false);
  }

  /*
   * Comments
   */
  /\(\*/ {
    comment_face (true);
    language_print ($0);
    call (sml_comment);
    comment_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

/*
 * Binding tyvar seqs
 */
state sml_tyvarseq
{
  /,/ {
    language_print($0);
  }
  sml_tyvar_re {
    type_face(true);
    language_print($0);
    type_face(false);
  }
  /\)/ {
    language_print($0);
    return;
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/*
Local variables:
mode: c
End:
*/

/**
 * Name: sml_fancy
 * Description: Standard ML programming language.
 * Author: Matthew Fluet <mfluet@acm.org>
 */

/*
builtin_face        --- 
comment_face        --- comments
function_name_face  --- modules keywords
highlight_face      --- 
keyword_face        --- core keywords
reference_face      --- special constants (nums)
string_face         --- strings
type_face           --- type bindings
variable_name_face  --- constructor bindings
*/

/*
require_state (sml_verbose);
*/

TRUE = 1;
FALSE = 0;

sml_typctxt = -1;
sml_typctxt_expected_eqs = -1;
SML_DATBIND_UNKNOWN = 1;
SML_DATBIND_REPL = 2;
SML_DATBIND_DECL = 3;
sml_datbind = FALSE;
sml_exbind = FALSE;
sml_conbind = FALSE;

SML_AND_NUL = -1;
SML_AND_VALBIND = 1;
SML_AND_TYPBIND = 2;
SML_AND_DATBIND = 3;
SML_AND_EXBIND = 4;
SML_AND_STRBIND = 5;
SML_AND_SIGBIND = 6;
SML_AND_FUNBIND = 7;
SML_AND_WHERETYPE = 8;
sml_andbind = list (SML_AND_NUL);

SML_CORE_LEVEL = 0;
SML_MODULES_LEVEL = 1;

sml_endmatch = list (SML_MODULES_LEVEL);

sml_last_keyword = "";
sub sml_keyword (s, lvl) {
  sml_last_keyword = s;
  if (lvl == SML_CORE_LEVEL)
    keyword_face (true);
  else if (lvl = SML_MODULES_LEVEL)
    function_name_face (true);
  language_print (s);
  if (lvl == SML_CORE_LEVEL)
    keyword_face (false);
  else if (lvl = SML_MODULES_LEVEL)
    function_name_face (false);
}

sml_scopes = list (list ("",SML_MODULES_LEVEL));
sml_scope = 0;
sub sml_enter_scope (s, lvl) {
  sml_andbind = list(SML_AND_NUL, sml_andbind);
  sml_endmatch = list(-1, sml_endmatch);
  sml_scopes = list (list (s,lvl), sml_scopes);
  sml_scope++;
  return;
}
sub sml_leave_scope () {
  sml_scope--;
  sml_scopes = sml_scopes[1];
  sml_endmatch = sml_endmatch[1];
  sml_andbind = sml_andbind[1];
  return;
}
sub sml_let_level () {
  local scopes = sml_scopes;
  if (sml_andbind[0] == SML_AND_STRBIND ||
      sml_andbind[0] == SML_AND_FUNBIND) {
    return TRUE;
  }
  if ((strcmp(scopes[0][0],"let") == 0 ||
       strcmp(scopes[0][0],"(") == 0) &&
      scopes[0][1] == SML_MODULES_LEVEL)
    return TRUE;
  return FALSE;
}
sub sml_local_level () {
  local scopes = sml_scopes;

  if ((strcmp(scopes[0][0],"let") == 0 ||
       strcmp(scopes[0][0],"(") == 0) &&
      scopes[0][1] == SML_MODULES_LEVEL)
    return TRUE;
  while (length(scopes) == 2) {
    if (strcmp(scopes[0][0],"local") == 0 &&
        scopes[0][1] == SML_MODULES_LEVEL) 
      scopes = scopes[1];
    else if (strcmp(scopes[0][0],"let") == 0 &&
             scopes[0][1] == SML_MODULES_LEVEL) 
      return TRUE;
    else 
      return FALSE;
  }
  return TRUE;
}

sub sml_start_typctxt (eqs) {
  type_face(true);
  sml_typctxt = sml_scope;
  sml_typctxt_expected_eqs = eqs;
  return;
}
sub sml_start_typbind () {
  sml_start_typctxt (1);
  sml_datbind = FALSE;
  sml_exbind = FALSE;
  sml_conbind = FALSE;
}
sub sml_start_eqtyp () {
  sml_start_typctxt (0);
  sml_datbind = FALSE;
  sml_exbind = FALSE;
  sml_conbind = FALSE;
}
sub sml_start_wheretyp () {
  sml_start_typctxt (1);
  sml_datbind = FALSE;
  sml_exbind = FALSE;
  sml_conbind = FALSE;
}
sub sml_start_sharingtyp () {
  sml_start_typctxt (-1);
  sml_datbind = FALSE;
  sml_exbind = FALSE;
  sml_conbind = FALSE;
}
sub sml_start_datbind () {
  sml_start_typctxt (1);
  sml_datbind = SML_DATBIND_UNKNOWN;
  sml_exbind = FALSE;
  sml_conbind = FALSE;
}
sub sml_start_exbind () {
  sml_start_typctxt (1);
  sml_datbind = FALSE;
  sml_exbind = TRUE;
  sml_conbind = TRUE;
}
sub sml_finish_typctxt () {
  if (sml_typctxt == sml_scope) {
    sml_typctxt = -1;
    sml_typctxt_expected_eqs = -1;
    if (sml_datbind) {sml_datbind = FALSE; sml_conbind = FALSE;}
    if (sml_exbind) {sml_exbind = FALSE; sml_conbind = FALSE;}
    type_face (false);
  }
  return;
}


state sml_fancy /* extends sml_verbose */
{
  /*
   * Keywords
   */
  /(:|,|_|->)/ {
    language_print ($0);
  }
  /[\({[]/ {
    sml_enter_scope ($0,sml_scopes[0][1]);
    language_print ($0);
  }
  /[]}\)]/ {
    sml_finish_typctxt ();
    language_print ($0);
    sml_leave_scope ();
  }
  /(\.\.\.|;|=>)/ {
    sml_finish_typctxt ();
    sml_andbind[0] = SML_AND_NUL;
    language_print ($0);
  }
  /\|/ {
    if (sml_datbind == SML_DATBIND_DECL) {
      type_face (false);
      language_print ($0);
      sml_conbind = TRUE;
      type_face (true);
    } else { 
      language_print ($0);
    }
  }
  /=/ {
    if (sml_typctxt != -1) {
      type_face (false);
      language_print ($0);
      type_face (true);
      if (sml_typctxt_expected_eqs == 0) {
        sml_finish_typctxt ();
      } else {
        sml_typctxt_expected_eqs--;
        if (sml_datbind == SML_DATBIND_UNKNOWN) {
          sml_conbind = TRUE;
        }
        if (sml_exbind) {
          sml_conbind = TRUE;
        }
      }
    } else {
      language_print ($0);
    }
  }
  sml_sel_re {
    language_print ($0);
  }
  /\b(abstype)\b/ { 
    sml_finish_typctxt ();
    sml_enter_scope ($0,SML_CORE_LEVEL);
    sml_keyword ($0, SML_CORE_LEVEL); 
    sml_andbind[0] = SML_AND_DATBIND;
    sml_endmatch[0] = SML_CORE_LEVEL;
    sml_start_datbind ();
  }
  /\b(and)\b/ { 
    sml_finish_typctxt ();
    if (sml_andbind[0] == SML_AND_VALBIND) {
      sml_keyword ($0, SML_CORE_LEVEL); 
    } else if (sml_andbind[0] == SML_AND_TYPBIND) {
      sml_keyword ($0, SML_CORE_LEVEL); 
      sml_start_typbind ();
    } else if (sml_andbind[0] == SML_AND_DATBIND) {
      sml_keyword ($0, SML_CORE_LEVEL); 
      sml_start_datbind ();
    } else if (sml_andbind[0] == SML_AND_EXBIND) {
      sml_keyword ($0, SML_CORE_LEVEL); 
      sml_start_exbind ();
    } else if (sml_andbind[0] == SML_AND_STRBIND) {
      sml_keyword ($0, SML_MODULES_LEVEL); 
    } else if (sml_andbind[0] == SML_AND_SIGBIND) {
      sml_keyword ($0, SML_MODULES_LEVEL); 
    } else if (sml_andbind[0] == SML_AND_FUNBIND) {
      sml_keyword ($0, SML_MODULES_LEVEL); 
    } else if (sml_andbind[0] == SML_AND_WHERETYPE) {
      sml_keyword ($0, SML_MODULES_LEVEL); 
      sml_last_keyword = "where";
    }
      
  }
  /\b(andalso)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(as)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(case)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(datatype)\b/ { 
    if (sml_datbind == SML_DATBIND_UNKNOWN) {
      sml_datbind = SML_DATBIND_REPL;
      sml_conbind = FALSE;
      sml_keyword ($0, SML_CORE_LEVEL);
    } else {
      sml_finish_typctxt ();
      sml_keyword ($0, SML_CORE_LEVEL); 
      sml_andbind[0] = SML_AND_DATBIND;
      sml_start_datbind ();
    }
  }
  /\b(do)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(else)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(end)\b/ {
    sml_finish_typctxt ();
    sml_keyword ($0, sml_endmatch[0]); 
    sml_leave_scope ();
  }
  /\b(eqtype)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL); 
    sml_start_eqtyp ();
  }
  /\b(exception)\b/ {  
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL); 
    sml_andbind[0] = SML_AND_EXBIND;
    sml_start_exbind ();
  }
  /\b(fn)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  sml_fun_with_tyvar_re {
    sml_finish_typctxt ();
    sml_keyword ($1, SML_CORE_LEVEL);
    language_print ($2);
    type_face(true);
    language_print ($3);
    type_face(false);
    sml_andbind[0] = SML_AND_VALBIND;
  }
  sml_fun_with_tyvarseq_re {
    sml_finish_typctxt ();
    sml_keyword ($1, SML_CORE_LEVEL);
    language_print ($2);
    language_print ($3);
    language_print ($4);
    type_face(true);
    language_print ($5);
    type_face(false);
    call (sml_tyvarseq);
    sml_andbind[0] = SML_AND_VALBIND;
  }
  /\b(fun)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL); 
    sml_andbind[0] = SML_AND_VALBIND;
  }
  /\b(functor)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_MODULES_LEVEL); 
    sml_andbind[0] = SML_AND_FUNBIND;
  }
  /\b(handle)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(if)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(in)\b/ { 
    sml_finish_typctxt ();
    sml_andbind[0] = SML_AND_NUL;
    sml_keyword ($0, sml_endmatch[0]); 
  }
  /\b(include)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_MODULES_LEVEL); 
  }
  sml_infix_re {
    sml_finish_typctxt ();
    sml_keyword ($1, SML_CORE_LEVEL); 
    language_print ($3);
    language_print ($4);
  }
  /\b(let)\b/ { 
    sml_finish_typctxt ();
    if (sml_let_level()) {
      sml_enter_scope ($0,SML_MODULES_LEVEL);
      sml_keyword ($0, SML_MODULES_LEVEL); 
      sml_endmatch[0] = SML_MODULES_LEVEL;
    } else {
      sml_enter_scope ($0,SML_CORE_LEVEL);
      sml_keyword ($0, SML_CORE_LEVEL); 
      sml_endmatch[0] = SML_CORE_LEVEL;
    }
  }
  /\b(local)\b/ { 
    sml_finish_typctxt ();
    if (sml_local_level ()) {
      sml_enter_scope ($0, SML_MODULES_LEVEL);
      sml_keyword ($0, SML_MODULES_LEVEL); 
      sml_endmatch[0] = SML_MODULES_LEVEL;
    } else {
      sml_enter_scope ($0, SML_CORE_LEVEL);
      sml_keyword ($0, SML_CORE_LEVEL); 
      sml_endmatch[0] = SML_CORE_LEVEL;
    }
  }
  /\b(nonfix)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(of)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(op)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(open)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_MODULES_LEVEL); 
  }
  /\b(orelse)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(raise)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(rec)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(sharing)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_MODULES_LEVEL); 
  }
  /\b(sig)\b/ { 
    sml_enter_scope ($0, SML_CORE_LEVEL);
    sml_keyword ($0, SML_MODULES_LEVEL); 
    sml_endmatch[0] = SML_MODULES_LEVEL;
  }
  /\b(signature)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_MODULES_LEVEL); 
    sml_andbind[0] = SML_AND_SIGBIND;
  }
  /\b(struct)\b/ { 
    sml_enter_scope ($0, SML_CORE_LEVEL);
    sml_keyword ($0, SML_MODULES_LEVEL); 
    sml_endmatch[0] = SML_MODULES_LEVEL;
  }
  /\b(structure)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_MODULES_LEVEL); 
    sml_andbind[0] = SML_AND_STRBIND;
  }
  /\b(then)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(type)\b/ { 
    if (strcmp(sml_last_keyword,"where") == 0) {
      sml_keyword ($0, SML_MODULES_LEVEL);
      sml_last_keyword = "where type";
      sml_andbind[0] = SML_AND_WHERETYPE;
      sml_start_wheretyp ();
    } else if (strcmp(sml_last_keyword,"sharing") == 0) {
      sml_keyword ($0, SML_MODULES_LEVEL);
      sml_last_keyword = "sharing type";
      sml_andbind[0] = SML_AND_NUL;
      sml_start_sharingtyp ();
    } else {
      sml_finish_typctxt ();
      sml_keyword ($0, SML_CORE_LEVEL);  
      sml_andbind[0] = SML_AND_TYPBIND;
      sml_start_typbind ();
    }
  }
  sml_val_with_tyvar_re {
    sml_finish_typctxt ();
    sml_keyword ($1, SML_CORE_LEVEL);
    language_print ($2);
    type_face(true);
    language_print ($3);
    type_face(false);
    sml_andbind[0] = SML_AND_VALBIND;
  }
  sml_val_with_tyvarseq_re {
    sml_finish_typctxt ();
    sml_keyword ($1, SML_CORE_LEVEL);
    language_print ($2);
    language_print ($3);
    language_print ($4);
    type_face(true);
    language_print ($5);
    type_face(false);
    call (sml_tyvarseq);
    sml_andbind[0] = SML_AND_VALBIND;
  }
  /\b(val)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL);  
    sml_andbind[0] = SML_AND_VALBIND;
  }
  /\b(where)\b/ { 
    sml_keyword ($0, SML_MODULES_LEVEL); 
  }
  /\b(while)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(with)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(withtype)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL); 
    sml_andbind[0] = SML_AND_TYPBIND;
    sml_start_typbind ();
  }
  sml_longid_re {
    if (sml_conbind) {
      sml_conbind = FALSE;
      variable_name_face (true);
      language_print ($0);
      variable_name_face (false);      
    } else {
      language_print ($0);
    }
  }
  sml_id_re {
    if (sml_conbind) {
      if (sml_datbind == SML_DATBIND_UNKNOWN)
        sml_datbind = SML_DATBIND_DECL;
      sml_conbind = FALSE;
      variable_name_face (true);
      language_print ($0);
      variable_name_face (false);
    } else {
      language_print ($0);
    }
  }

  /* extends sml_verbose */
  sml_infix_re {
    keyword_face (true);
    language_print ($1);
    keyword_face (false);
    language_print ($3);
    language_print ($4);
  }
  sml_sel_re {
    language_print ($0);
  }

  /*
   * Special constants (nums)
   */
  sml_scon_num_re {
    reference_face (true);
    language_print ($0);
    reference_face (false);
  }

  /* extend sml_simple */
  BEGIN {
    header ();
  }
  END {
    sml_finish_typctxt ();
    trailer ();
  }

  /*
   * Keywords
   */
  sml_keywords_re {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  /*
   * Special constants (strings)
   */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (sml_string);
    string_face (false);
  }

  /*
   * Special constants (chars)
   */
  /(#)(\")/ {
    language_print ($1);
    string_face (true);
    language_print ($2);
    call (sml_string);
    string_face (false);
  }

  /*
   * Comments
   */
  /\(\*/ {
    comment_face (true);
    language_print ($0);
    call (sml_comment);
    comment_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/*
Local variables:
mode: c
End:
*/

/**
 * Name: sml_gaudy
 * Description: Standard ML programming language.
 * Author: Matthew Fluet <mfluet@acm.org>
 */

/*
builtin_face        --- 
comment_face        --- comments
function_name_face  --- modules keywords
highlight_face      --- 
keyword_face        --- core keywords
reference_face      --- special constants (nums)
string_face         --- strings
type_face           --- type bindings, type annotations
variable_name_face  --- constructor bindings
*/

/*
require_state (sml_fancy);
*/

state sml_gaudy /* extends sml_fancy */
{
  /*
   * Keywords
   */
  /,/ {
    sml_finish_typctxt ();
    language_print ($0);
  }
  /:/ {
    if (sml_andbind[0] == SML_AND_STRBIND ||
        sml_andbind[0] == SML_AND_FUNBIND) {
      language_print ($0);
    } else {
      language_print ($0);
      if (sml_typctxt == -1)
        sml_start_typctxt (0);
    }
  }

  /* extends sml_fancy */
  /*
   * Keywords
   */
  /(:|,|_|->)/ {
    language_print ($0);
  }
  /[\({[]/ {
    sml_enter_scope ($0,sml_scopes[0][1]);
    language_print ($0);
  }
  /[]}\)]/ {
    sml_finish_typctxt ();
    language_print ($0);
    sml_leave_scope ();
  }
  /(\.\.\.|;|=>)/ {
    sml_finish_typctxt ();
    sml_andbind[0] = SML_AND_NUL;
    language_print ($0);
  }
  /\|/ {
    if (sml_datbind == SML_DATBIND_DECL) {
      type_face (false);
      language_print ($0);
      sml_conbind = TRUE;
      type_face (true);
    } else { 
      language_print ($0);
    }
  }
  /=/ {
    if (sml_typctxt != -1) {
      type_face (false);
      language_print ($0);
      type_face (true);
      if (sml_typctxt_expected_eqs == 0) {
        sml_finish_typctxt ();
      } else {
        sml_typctxt_expected_eqs--;
        if (sml_datbind == SML_DATBIND_UNKNOWN) {
          sml_conbind = TRUE;
        }
        if (sml_exbind) {
          sml_conbind = TRUE;
        }
      }
    } else {
      language_print ($0);
    }
  }
  sml_sel_re {
    language_print ($0);
  }
  /\b(abstype)\b/ { 
    sml_finish_typctxt ();
    sml_enter_scope ($0,SML_CORE_LEVEL);
    sml_keyword ($0, SML_CORE_LEVEL); 
    sml_andbind[0] = SML_AND_DATBIND;
    sml_endmatch[0] = SML_CORE_LEVEL;
    sml_start_datbind ();
  }
  /\b(and)\b/ { 
    sml_finish_typctxt ();
    if (sml_andbind[0] == SML_AND_VALBIND) {
      sml_keyword ($0, SML_CORE_LEVEL); 
    } else if (sml_andbind[0] == SML_AND_TYPBIND) {
      sml_keyword ($0, SML_CORE_LEVEL); 
      sml_start_typbind ();
    } else if (sml_andbind[0] == SML_AND_DATBIND) {
      sml_keyword ($0, SML_CORE_LEVEL); 
      sml_start_datbind ();
    } else if (sml_andbind[0] == SML_AND_EXBIND) {
      sml_keyword ($0, SML_CORE_LEVEL); 
      sml_start_exbind ();
    } else if (sml_andbind[0] == SML_AND_STRBIND) {
      sml_keyword ($0, SML_MODULES_LEVEL); 
    } else if (sml_andbind[0] == SML_AND_SIGBIND) {
      sml_keyword ($0, SML_MODULES_LEVEL); 
    } else if (sml_andbind[0] == SML_AND_FUNBIND) {
      sml_keyword ($0, SML_MODULES_LEVEL); 
    } else if (sml_andbind[0] == SML_AND_WHERETYPE) {
      sml_keyword ($0, SML_MODULES_LEVEL); 
      sml_last_keyword = "where";
    }
      
  }
  /\b(andalso)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(as)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(case)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(datatype)\b/ { 
    if (sml_datbind == SML_DATBIND_UNKNOWN) {
      sml_datbind = SML_DATBIND_REPL;
      sml_conbind = FALSE;
      sml_keyword ($0, SML_CORE_LEVEL);
    } else {
      sml_finish_typctxt ();
      sml_keyword ($0, SML_CORE_LEVEL); 
      sml_andbind[0] = SML_AND_DATBIND;
      sml_start_datbind ();
    }
  }
  /\b(do)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(else)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(end)\b/ {
    sml_finish_typctxt ();
    sml_keyword ($0, sml_endmatch[0]); 
    sml_leave_scope ();
  }
  /\b(eqtype)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL); 
    sml_start_eqtyp ();
  }
  /\b(exception)\b/ {  
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL); 
    sml_andbind[0] = SML_AND_EXBIND;
    sml_start_exbind ();
  }
  /\b(fn)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  sml_fun_with_tyvar_re {
    sml_finish_typctxt ();
    sml_keyword ($1, SML_CORE_LEVEL);
    language_print ($2);
    type_face(true);
    language_print ($3);
    type_face(false);
    sml_andbind[0] = SML_AND_VALBIND;
  }
  sml_fun_with_tyvarseq_re {
    sml_finish_typctxt ();
    sml_keyword ($1, SML_CORE_LEVEL);
    language_print ($2);
    language_print ($3);
    language_print ($4);
    type_face(true);
    language_print ($5);
    type_face(false);
    call (sml_tyvarseq);
    sml_andbind[0] = SML_AND_VALBIND;
  }
  /\b(fun)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL); 
    sml_andbind[0] = SML_AND_VALBIND;
  }
  /\b(functor)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_MODULES_LEVEL); 
    sml_andbind[0] = SML_AND_FUNBIND;
  }
  /\b(handle)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(if)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(in)\b/ { 
    sml_finish_typctxt ();
    sml_andbind[0] = SML_AND_NUL;
    sml_keyword ($0, sml_endmatch[0]); 
  }
  /\b(include)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_MODULES_LEVEL); 
  }
  sml_infix_re {
    sml_finish_typctxt ();
    sml_keyword ($1, SML_CORE_LEVEL); 
    language_print ($3);
    language_print ($4);
  }
  /\b(let)\b/ { 
    sml_finish_typctxt ();
    if (sml_let_level()) {
      sml_enter_scope ($0,SML_MODULES_LEVEL);
      sml_keyword ($0, SML_MODULES_LEVEL); 
      sml_endmatch[0] = SML_MODULES_LEVEL;
    } else {
      sml_enter_scope ($0,SML_CORE_LEVEL);
      sml_keyword ($0, SML_CORE_LEVEL); 
      sml_endmatch[0] = SML_CORE_LEVEL;
    }
  }
  /\b(local)\b/ { 
    sml_finish_typctxt ();
    if (sml_local_level ()) {
      sml_enter_scope ($0, SML_MODULES_LEVEL);
      sml_keyword ($0, SML_MODULES_LEVEL); 
      sml_endmatch[0] = SML_MODULES_LEVEL;
    } else {
      sml_enter_scope ($0, SML_CORE_LEVEL);
      sml_keyword ($0, SML_CORE_LEVEL); 
      sml_endmatch[0] = SML_CORE_LEVEL;
    }
  }
  /\b(nonfix)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(of)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(op)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(open)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_MODULES_LEVEL); 
  }
  /\b(orelse)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(raise)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(rec)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(sharing)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_MODULES_LEVEL); 
  }
  /\b(sig)\b/ { 
    sml_enter_scope ($0, SML_CORE_LEVEL);
    sml_keyword ($0, SML_MODULES_LEVEL); 
    sml_endmatch[0] = SML_MODULES_LEVEL;
  }
  /\b(signature)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_MODULES_LEVEL); 
    sml_andbind[0] = SML_AND_SIGBIND;
  }
  /\b(struct)\b/ { 
    sml_enter_scope ($0, SML_CORE_LEVEL);
    sml_keyword ($0, SML_MODULES_LEVEL); 
    sml_endmatch[0] = SML_MODULES_LEVEL;
  }
  /\b(structure)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_MODULES_LEVEL); 
    sml_andbind[0] = SML_AND_STRBIND;
  }
  /\b(then)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(type)\b/ { 
    if (strcmp(sml_last_keyword,"where") == 0) {
      sml_keyword ($0, SML_MODULES_LEVEL);
      sml_last_keyword = "where type";
      sml_andbind[0] = SML_AND_WHERETYPE;
      sml_start_wheretyp ();
    } else if (strcmp(sml_last_keyword,"sharing") == 0) {
      sml_keyword ($0, SML_MODULES_LEVEL);
      sml_last_keyword = "sharing type";
      sml_andbind[0] = SML_AND_NUL;
      sml_start_sharingtyp ();
    } else {
      sml_finish_typctxt ();
      sml_keyword ($0, SML_CORE_LEVEL);  
      sml_andbind[0] = SML_AND_TYPBIND;
      sml_start_typbind ();
    }
  }
  sml_val_with_tyvar_re {
    sml_finish_typctxt ();
    sml_keyword ($1, SML_CORE_LEVEL);
    language_print ($2);
    type_face(true);
    language_print ($3);
    type_face(false);
    sml_andbind[0] = SML_AND_VALBIND;
  }
  sml_val_with_tyvarseq_re {
    sml_finish_typctxt ();
    sml_keyword ($1, SML_CORE_LEVEL);
    language_print ($2);
    language_print ($3);
    language_print ($4);
    type_face(true);
    language_print ($5);
    type_face(false);
    call (sml_tyvarseq);
    sml_andbind[0] = SML_AND_VALBIND;
  }
  /\b(val)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL);  
    sml_andbind[0] = SML_AND_VALBIND;
  }
  /\b(where)\b/ { 
    sml_keyword ($0, SML_MODULES_LEVEL); 
  }
  /\b(while)\b/ { 
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(with)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL); 
  }
  /\b(withtype)\b/ { 
    sml_finish_typctxt ();
    sml_keyword ($0, SML_CORE_LEVEL); 
    sml_andbind[0] = SML_AND_TYPBIND;
    sml_start_typbind ();
  }
  sml_longid_re {
    if (sml_conbind) {
      sml_conbind = FALSE;
      variable_name_face (true);
      language_print ($0);
      variable_name_face (false);      
    } else {
      language_print ($0);
    }
  }
  sml_id_re {
    if (sml_conbind) {
      if (sml_datbind == SML_DATBIND_UNKNOWN)
        sml_datbind = SML_DATBIND_DECL;
      sml_conbind = FALSE;
      variable_name_face (true);
      language_print ($0);
      variable_name_face (false);
    } else {
      language_print ($0);
    }
  }

  /* extends sml_verbose */
  sml_infix_re {
    keyword_face (true);
    language_print ($1);
    keyword_face (false);
    language_print ($3);
    language_print ($4);
  }
  sml_sel_re {
    language_print ($0);
  }

  /*
   * Special constants (nums)
   */
  sml_scon_num_re {
    reference_face (true);
    language_print ($0);
    reference_face (false);
  }

  /* extend sml_simple */
  BEGIN {
    header ();
  }
  END {
    sml_finish_typctxt ();
    trailer ();
  }

  /*
   * Keywords
   */
  sml_keywords_re {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  /*
   * Special constants (strings)
   */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (sml_string);
    string_face (false);
  }

  /*
   * Special constants (chars)
   */
  /(#)(\")/ {
    language_print ($1);
    string_face (true);
    language_print ($2);
    call (sml_string);
    string_face (false);
  }

  /*
   * Comments
   */
  /\(\*/ {
    comment_face (true);
    language_print ($0);
    call (sml_comment);
    comment_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/*
Local variables:
mode: c
End:
*/
