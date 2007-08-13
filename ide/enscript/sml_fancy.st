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

require_state (sml_verbose);

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

state sml_fancy extends sml_verbose
{
  END {
    sml_finish_typctxt ();
  }

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
    sml_finish_typctxt ();
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

}

/*
 * Binding tyvar seqs
 */
state sml_tyvarseq extends Highlight 
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
}



/*
Local variables:
mode: c
End:
*/
