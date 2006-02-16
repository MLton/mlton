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

state sml_simple extends HighlightEntry
{
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
}

/*
 * Strings
 */
state sml_string extends Highlight
{
  /\\\\./ {
    language_print ($0);
  }

  /\"/ {
    language_print ($0);
    return;
  }
}

/*
 * Nested comments
 */
state sml_comment extends Highlight 
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
}


/*
Local variables:
mode: c
End:
*/
