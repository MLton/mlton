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

require_state (sml_simple);

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


state sml_verbose extends sml_simple 
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
}


/*
Local variables:
mode: c
End:
*/
