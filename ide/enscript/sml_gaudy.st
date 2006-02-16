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

require_state (sml_fancy);

state sml_gaudy extends sml_fancy
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
}


/*
Local variables:
mode: c
End:
*/
