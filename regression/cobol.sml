





val condition_names:(string list ref) = ref []



(*hojfeld defunctorised*)
structure Cobol = struct

  type pos = unit (*hojfeld*)
(* BASIC STUFF *)
  type ts2k = string

  datatype figurative_constant =
    ZERO
  | ZEROS
  | ZEROES
  | SPACE
  | SPACES
  | HIGHVALUE
  | HIGHVALUES
  | LOWVALUE
  | LOWVALUES
  | QUOTE
  | QUOTES
  | ALL_ZERO
  | ALL_ZEROS
  | ALL_ZEROES
  | ALL_SPACE
  | ALL_SPACES
  | ALL_HIGHVALUE
  | ALL_HIGHVALUES
  | ALL_LOWVALUE
  | ALL_LOWVALUES
  | ALL_QUOTE
  | ALL_QUOTES
  | ALL_NONNUMERICLITERAL of string

  datatype literal =
    LITERAL_IS_NONNUMERICLITERAL of string * pos * pos
  | LITERAL_IS_INTEGER of string * pos * pos
  | LITERAL_IS_DECIMALNUMBER of string * pos * pos
  | LITERAL_IS_BOOLEANLITERAL of bool * pos * pos
  | LITERAL_IS_FIGURATIVECONSTANT of figurative_constant * pos * pos

  datatype data_name_or_literal =
    DATA_NAME_OR_LITERAL_IS_DATA_NAME of string
  | DATA_NAME_OR_LITERAL_IS_LITERAL of literal

  datatype data_names =
    DATA_NAMES of data_names * string
  | ONE_DATA_NAME of string

  datatype data_name_or_integer =
    DATA_NAME_OR_INTEGER_IS_DATA_NAME of string
  | DATA_NAME_OR_INTEGER_IS_INTEGER of string

  type qualification = string

  datatype qualifications = 
    SEVERAL_QUALIFICATIONS of qualifications * qualification
  | NO_QUALIFICATIONS

  type subscript_phrase = unit (* TODO *)

  datatype special_register =
    LINAGECOUNTER
  | DEBUGITEM

  datatype identifier =
    QUALIFIED_IDENTIFIER of string * qualifications * subscript_phrase
                            * pos * pos
  | SPECIAL_REGISTER of special_register * pos * pos

  datatype identifiers =
    SEVERAL_IDENTIFIERS of identifiers * identifier
  | ONE_IDENTIFIER of identifier

  datatype identifier_or_literal =
    IDENTIFIER_OR_LITERAL_IS_IDENTIFIER of identifier
  | IDENTIFIER_OR_LITERAL_IS_LITERAL of literal

  datatype identifier_or_literals =
    SEVERAL_IDENTIFIER_OR_LITERALS of identifier_or_literals
                                    * identifier_or_literal
  | ONE_IDENTIFIER_OR_LITERAL of identifier_or_literal

  datatype rounded =
    ROUNDED
  | NOT_ROUNDED

  datatype identifier_rounded =
    IDENTIFIER_ROUNDED of identifier * rounded

  datatype identifier_roundeds =
    SEVERAL_IDENTIFIER_ROUNDEDS of identifier_roundeds * identifier_rounded
  | ONE_IDENTIFIER_ROUNDED of identifier_rounded

  datatype is_not =
    IS_NOT_IS
  | IS_NOT_NOT

  datatype data_class =
    DATA_CLASS_IS_NUMERIC
  | DATA_CLASS_IS_ALPHABETIC

  datatype relational_operator =
    GREATER
  | LESS
  | EQUAL
  | GREATER_OR_EQUAL
  | LESS_OR_EQUAL

  datatype sign_specification = 
    SIGN_SPECIFICATION_IS_POSITIVE
  | SIGN_SPECIFICATION_IS_NEGATIVE
  | SIGN_SPECIFICATION_IS_ZERO

(* END OF BASIC STUFF *)

(* ENVIRONMENT DIVISION *)

  type source_computer_paragraph = unit
  type object_computer_paragraph = unit

  datatype currency_sign_clause =
    CURRENCY of string

  datatype decimal_clause =
    DECIMALPOINT_IS_COMMA

  datatype special_names_clause =
    ENVIRONMENT_CLAUSE
  | ALPHABET_CLAUSE
  | CURRENCY_SIGN_CLAUSE of currency_sign_clause
  | DECIMAL_CLAUSE of decimal_clause

  datatype special_names_clauses =
    SEVERAL_SPECIAL_NAMES_CLAUSES of special_names_clauses
                                   * special_names_clause
  | NO_SPECIAL_NAMES_CLAUSES

  datatype special_names_paragraph =
    SPECIAL_NAMES_PARAGRAPH of special_names_clauses
  | NO_SPECIAL_NAMES_PARAGRAPH

  datatype configuration_section =
    CONFIGURATION_SECTION of
        source_computer_paragraph
      * object_computer_paragraph
      * special_names_paragraph

  type input_output_section = unit

(* END OF ENVIRONMENT DIVISION *)

(* WORKINGSTORAGE and LINKAGE SECTION *)

  datatype left_right =
    LEFT
  | RIGHT

  datatype through_data_name =
    THROUGH_DATA_NAME of string
  | NOT_THROUGH_DATA_NAME

  datatype separate_character =
    SEPARATE
  | NO_SEPARATE

  datatype leading_trailing =
    LEADING_TRAILING_IS_LEADING
  | LEADING_TRAILING_IS_TRAILING
  datatype table_length_description =
    FIXED_LENGTH_TABLE of string      (* integer-2 *)
  | VARIABLE_LENGTH_TABLE of string   (* integer-1 *)
                           * string   (* integer-2 *)
                           * string   (* data-name-1 *)

  datatype ascending_descending =
    ASCENDING
  | DESCENDING

  datatype ascending_descending_key_phrase =
    ASCENDING_DESCENDING_KEY_PHRASE of ascending_descending *
                                       data_names

  datatype ascending_descending_key_phrases =
    ASCENDING_DESCENDING_KEY_PHRASES of ascending_descending_key_phrases *
                                        ascending_descending_key_phrase
  | NO_ASCENDING_DESCENDING_KEY_PHRASES

  datatype index_names =
    SEVERAL_INDEX_NAMES of index_names * string
  | ONE_INDEX_NAME of string

  datatype indexed_by_phrase =
    INDEXED_BY_PHRASE of index_names
  | NO_INDEXED_BY_PHRASE

  datatype usage_specifier =
    USAGE_IS_DISPLAY
  | USAGE_IS_INDEX
  | USAGE_IS_COMP3
  | USAGE_IS_COMP4
  | USAGE_IS_COMP

  datatype through_literal =
    THROUGH_LITERAL of literal
  | NO_THROUGH_LITERAL

  datatype literal_range =
    LITERAL_RANGE of literal * through_literal

  datatype literal_ranges =
    SEVERAL_LITERAL_RANGES of literal_ranges * literal_range
  | ONE_LITERAL_RANGE of literal_range

  datatype data_description_clause =
    DATA_DESCRIPTION_CLAUSE_IS_USAGE_CLAUSE of usage_specifier
                    * pos * pos
  | DATA_DESCRIPTION_CLAUSE_IS_SIGN_CLAUSE of leading_trailing *
                                              separate_character
                    * pos * pos
  | DATA_DESCRIPTION_CLAUSE_IS_OCCURS_CLAUSE
       of table_length_description *
          ascending_descending_key_phrases *
          indexed_by_phrase
                    * pos * pos
  | DATA_DESCRIPTION_CLAUSE_IS_SYNCHRONIZED_CLAUSE of left_right
                    * pos * pos
  | DATA_DESCRIPTION_CLAUSE_IS_JUSTIFIED_CLAUSE of
                    pos * pos
  | DATA_DESCRIPTION_CLAUSE_IS_BLANK_WHEN_ZERO_CLAUSE of
                    pos * pos
  | DATA_DESCRIPTION_CLAUSE_IS_VALUE_CLAUSE of literal_ranges
                    * pos * pos
  | DATA_DESCRIPTION_CLAUSE_IS_PICTURE_CLAUSE of string
                    * pos * pos
  | DATA_DESCRIPTION_CLAUSE_IS_INDICATOR_CLAUSE of string
                    * pos * pos

  datatype data_description_clauses =
    DATA_DESCRIPTION_CLAUSES of data_description_clauses *
                                data_description_clause
  | NO_DATA_DESCRIPTION_CLAUSES

  datatype redefines_clause =
    REDEFINES_CLAUSE of string
  | NO_REDEFINES_CLAUSE

  datatype filler =
    FILLER
  | NO_FILLER

  datatype data_name_or_filler =
    DATA_NAME_OR_FILLER_IS_DATA_NAME of string
  | DATA_NAME_OR_FILLER_IS_FILLER of filler

  datatype record_description_entry =
    DATA_DESCRIPTION_ENTRY_134 of string               (* levelnumber *)
                                * data_name_or_filler
                                * redefines_clause
                                * data_description_clauses
                                * pos * pos
  | RENAMES_CLAUSE of string               (* levelnumber *)
                    * string               (* data-name *)
                    * string               (* data-name *)
                    * through_data_name
                    * pos * pos
  | TS2K_NOARROW of ts2k * pos * pos
  | TS2K_ARROW  of ts2k * ts2k * pos * pos
  | TS2K_ALL of ts2k * ts2k * pos * pos
  | TS2K_END of pos * pos
  | TS2K_ASSUME_SEPARATE of pos * pos

(* CHANGED-MHS: the 4 TS2K constructors were:
  | TS2K of ts2k * pos * pos
  | TS2K_EXPANDS of identifier * pos * pos
  | TS2K_FIRST_OR_FINAL
   END-CHANGED-MHS *)  

  datatype record_description_entries =
    RECORD_DESCRIPTION_ENTRIES of record_description_entries *
                                  record_description_entry
  | NO_RECORD_DESCRIPTION_ENTRIES

  datatype record_description_entries_opt =
    RECORD_DESCRIPTION_ENTRIES_OPT of record_description_entries
  | NO_RECORD_DESCRIPTION_ENTRIES_OPT

  datatype working_storage_section =
    WORKINGSTORAGE_SECTION of record_description_entries_opt * pos * pos
  | NO_WORKINGSTORAGE_SECTION

  datatype linkage_section =
    LINKAGE_SECTION of record_description_entries_opt * pos * pos
  | NO_LINKAGE_SECTION

(* END OF WORKINGSTORAGE and LINKAGE SECTION *)

(* FILE SECTION *)

  datatype integer_range =
    SIMPLE_RANGE of string
  | INTEGER_TO_INTEGER of string * string

  datatype record_contains_clause =
    RECORD_CONTAINS of integer_range

  datatype data_records_clause = 
    DATA_RECORD_IS of data_names

  datatype sort_description_clause =
    SORT_DESCRIPTION_CLAUSE_IS_RECORD of record_contains_clause
                    * pos * pos
  | SORT_DESCRIPTION_CLAUSE_IS_DATA of data_records_clause
                    * pos * pos

  datatype sort_description_clauses =
    SORT_DESCRIPTION_CLAUSES of sort_description_clauses *
                                sort_description_clause
  | NO_SORT_DESCRIPTION_CLAUSE

  datatype sort_description_entry =
    SD of string * sort_description_clauses

  datatype footing_specification =
    FOOTING of data_name_or_integer
  | NOFOOTING

  datatype top_bottom_specification =
    TOP
  | BOTTOM

  datatype top_bottom_specifications =
    TOP_BOTTOM_SPECIFICATIONS of top_bottom_specifications *
                                 top_bottom_specification
  | NO_TOP_BOTTOM_SPECIFICATION

  datatype value_of_phrase =
    VALUE_OF of string * data_name_or_literal

  datatype value_of_phrases =
    VALUE_OF_PHRASES of value_of_phrases * value_of_phrase
  | VALUE_OF_PHRASE of value_of_phrase

  datatype characters_or_records =
    CHARACTERS
  | RECORDS

  datatype file_description_clause =
    BLOCK of integer_range * characters_or_records
           * pos * pos
  | FILE_DESCRIPTION_CLAUSE_IS_RECORD of record_contains_clause
           * pos * pos
  | LABEL_STANDARD
  | LABEL_OMITTED
  | VALUE_OF_CLAUSE of value_of_phrases
           * pos * pos
  | FILE_DESCRIPTION_CLAUSE_IS_DATA of data_records_clause
           * pos * pos
  | LINAGE_CLAUSE of data_name_or_integer *
                     footing_specification *
                     top_bottom_specifications
           * pos * pos
  | CODE_SET_CLAUSE of string
           * pos * pos             

  datatype file_description_clauses =
    FILE_DESCRIPTION_CLAUSES of file_description_clauses *
                                file_description_clause
  | NO_FILE_DESCRIPTION_CLAUSES

  datatype file_description_entry =
    FD of string * file_description_clauses * pos * pos

  datatype file_and_sort_description_entry =
    FILE_DESCRIPTION_ENTRY of file_description_entry
  | SORT_DESCRIPTION_ENTRY of sort_description_entry
    
  datatype file_description_paragraph =
    FILE_DESCRIPTION_PARAGRAPH of file_and_sort_description_entry *
                                  record_description_entries

  datatype file_description_paragraphs =
    FILE_DESCRIPTION_PARAGRAPHS of file_description_paragraphs *
                                   file_description_paragraph
  | NO_FILE_DESCRIPTION_PARAGRAPHS

  datatype file_section =
    FILE_SECTION of file_description_paragraphs * pos * pos
  | NO_FILE_SECTION

(* END OF FILESECTION *)

(* STATEMENTS *)

  datatype arithmetic_expression =
    ARITHMETIC_EXPRESSION_IS_INTEGER of string * pos * pos
  | ARITHMETIC_EXPRESSION_IS_DECIMALNUMBER of string * pos * pos
  | ARITHMETIC_EXPRESSION_IS_NONNUMERICLITERAL of string * pos * pos
  | ARITHMETIC_EXPRESSION_IS_BOOLEANLITERAL of bool * pos * pos
  | ARITHMETIC_EXPRESSION_IS_FIGURATIVE_CONSTANT of figurative_constant *
                                                    pos * pos
  | ARITHMETIC_EXPRESSION_IS_IDENTIFIER of identifier
  | ARITHMETIC_EXPRESSION_IS_PLUS_SIGN of arithmetic_expression
  | ARITHMETIC_EXPRESSION_IS_MINUS_SIGN of arithmetic_expression
  | ARITHMETIC_EXPRESSION_IS_EXPONENTIATION of arithmetic_expression *
                                               arithmetic_expression
  | ARITHMETIC_EXPRESSION_IS_MULTIPLY of arithmetic_expression *
                                         arithmetic_expression
  | ARITHMETIC_EXPRESSION_IS_DIVIDE of arithmetic_expression *
                                       arithmetic_expression
  | ARITHMETIC_EXPRESSION_IS_ADD of arithmetic_expression *
                                    arithmetic_expression
  | ARITHMETIC_EXPRESSION_IS_SUBTRACT of arithmetic_expression *
                                         arithmetic_expression

  datatype simple_condition =
    CLASS_CONDITION of identifier * is_not * data_class
  | CONDITION_NAME of identifier
  | RELATION_CONDITION of arithmetic_expression *
                          is_not *
                          relational_operator *
                          arithmetic_expression * pos * pos
  (* Unclear if TRUE and FALSE are allowed in conditions  *)
  (* at all (TRUE is reserved word but not FALSE)         *)
  | SIMPLE_CONDITION_IS_TRUE of pos * pos
  | SIMPLE_CONDITION_IS_FALSE of pos * pos
  | SIGN_CONDITION of arithmetic_expression * is_not * sign_specification
  (* SWITCH_STATUS_CONDITION is SYSTEM-SHUT-DOWN or UPSI-i *)
  (* Don't think we need to know                           *)
  | SWITCH_STATUS_CONDITION of pos * pos

  datatype conditional_expression =
    SIMPLE_CONDITION of simple_condition
  | NEGATED_CONDITION of conditional_expression
    (* the manual says NOT simple_expression, but example in fig 11-12 *)
    (* tells a different story!!                                       *) 
  | AND_CONDITION of conditional_expression * conditional_expression
  | OR_CONDITION of conditional_expression * conditional_expression

  datatype add_or_subtract = ADD | SUBTRACT

  datatype date_day_time =
    DATE of pos * pos
  | DAY of pos * pos
  | TIME of pos * pos

  datatype identifier_or_literal_or_ddt =
    IDENTIFIER_OR_LITERAL_OR_DDT_IS_IDENTIFIER_OR_LITERAL of identifier_or_literal
  | IDENTIFIER_OR_LITERAL_OR_DDT_IS_DDT of date_day_time

  datatype annotation_statement =
    TS2K_ASSUME of identifier_or_literal_or_ddt * ts2k * pos * pos
  | TS2K_COERCE of identifier_or_literal_or_ddt * ts2k * pos * pos * identifier

  datatype move_statement =
    MOVE of identifier_or_literal * identifiers * pos * pos
  | MOVECORRESPONDING of identifier * identifier * pos * pos

  datatype from_environment =
    NO_FROM 
  | FROM of string * pos * pos

  datatype accept_statement =
    ACCEPT_ENVIRONMENT of identifier * from_environment * pos * pos
  | ACCEPT_DATE_DAY_TIME of identifier * date_day_time * pos * pos

  datatype statements =
    SEVERAL_STATEMENTS of statements * statement
  | ONE_STATEMENT of statement

  and statement =
    MOVE_STATEMENT of move_statement
  | COMPUTE_STATEMENT of compute_statement
  | ADD_OR_SUBTRACT_STATEMENT of add_or_subtract_statement
  | MULTIPLY_STATEMENT of multiply_statement
  | DIVIDE_STATEMENT of divide_statement
  | IF_STATEMENT of if_statement
  | ANNOTATION_STATEMENT of annotation_statement
  | ACCEPT_STATEMENT of accept_statement
  | OTHER_STATEMENT (* TODO *)

  and add_or_subtract_statement =
    ADD_OR_SUBTRACT of add_or_subtract * identifier_or_literals
         * identifier_roundeds
         * size_error_clause * pos * pos
  | ADD_OR_SUBTRACT_GIVING of add_or_subtract * identifier_or_literals
                * identifier_or_literal
                * identifier_roundeds
                * size_error_clause * pos * pos
  | ADD_OR_SUBTRACT_CORRESPONDING of add_or_subtract * identifier
                       * identifier
                       * rounded
                       * size_error_clause * pos * pos

  and compute_statement =
    COMPUTE of identifier_roundeds *
               arithmetic_expression *
               size_error_clause * pos * pos

  and multiply_statement =
    MULTIPLY of identifier_or_literal
         * identifier_roundeds
         * size_error_clause * pos * pos
  | MULTIPLY_GIVING of identifier_or_literal
                * identifier_or_literal
                * identifier_roundeds
                * size_error_clause * pos * pos

  (* NOTE: the following is not checked against the manual *)
  (*       neither is it tested                            *)
  and divide_statement =
  (* DIVIDE a INTO b [[ON] SIZE ERROR c]                         *)
    DIVIDE_INTO of identifier_or_literal                    (* a *)
                 * identifier_roundeds                      (* b *)
                 * size_error_clause                        (* c *)
                 * pos * pos
  (* DIVIDE a INTO b GIVING c [[ON] SIZE ERROR d]                *)
  | DIVIDE_INTO_GIVING of identifier_or_literal             (* a *)
                 * identifier_or_literal                    (* b *)
                 * identifier_roundeds                      (* c *)
                 * size_error_clause                        (* d *)
                 * pos * pos
  (* DIVIDE a INTO b GIVING c REMAINDER d [[ON] SIZE ERROR e]    *)
  | DIVIDE_INTO_GIVING_REMAINDER of identifier_or_literal   (* a *)
                 * identifier_or_literal                    (* b *)
                 * identifier_roundeds                      (* c *)
                 * identifier                               (* d *)
                 * size_error_clause                        (* e *)
                 * pos * pos
  (* DIVIDE a BY b GIVING c [[ON] SIZE ERROR d]                  *)
  | DIVIDE_BY_GIVING of identifier_or_literal               (* a *)
                 * identifier_or_literal                    (* b *)
                 * identifier_roundeds                      (* c *)
                 * size_error_clause                        (* d *)
                 * pos * pos
  (* DIVIDE a BY b GIVING c REMAINDER d [[ON] SIZE ERROR e]      *)
  | DIVIDE_BY_GIVING_REMAINDER of identifier_or_literal     (* a *)
                 * identifier_or_literal                    (* b *)
                 * identifier_roundeds                      (* c *)
                 * identifier                               (* d *)
                 * size_error_clause                        (* e *)
                 * pos * pos

  and size_error_clause =
    SIZE_ERROR of statements
  | NO_SIZE_ERROR

  and if_statement =
    IF of conditional_expression * then_statements * else_statements
          * pos * pos

  and then_statements =
    THEN_STATEMENTS of statements
  | NEXT_SENTENCE

  and else_statements =
    ELSE of statements
  | ELSE_NEXT_SENTENCE
  | NO_ELSE

(* ENDSTATEMENTS *)

(* PROCEDURE DIVISION *)

  type section_name = string
  type paragraph_name = string

  datatype sentence = SENTENCE of statements

  datatype sentences =
    SENTENCES of sentences * sentence
  | NO_SENTENCES

  datatype paragraph =
    PARAGRAPH of paragraph_name * sentences

  datatype paragraphs =
    SEVERAL_PARAGRAPHS of paragraphs * paragraph
  | ONE_PARAGRAPH of paragraph

  datatype segment_number_opt =
    SEGMENT_NUMBER of string
  | NOSEGMENT_NUMBER

  datatype sections =
    NO_BODY_SECTION of section_name * segment_number_opt
  | NO_BODY_SECTION_FOLLOWED_BY_SECTION of section_name *
                                           segment_number_opt *
                                           sections
  | SECTION of section_name *
               segment_number_opt *
               paragraphs_and_sections

  and paragraphs_and_sections =
    SEVERAL_PARAGRAPHS_AND_SECTIONS of paragraph * paragraphs_and_sections
  | SINGLE_PARAGRAPH_AND_SECTIONS of paragraph * sections
  | SINGLE_PARAGRAPH of paragraph


  type using_clause = unit (* TODO *)
  type declaratives_section = unit (* TODO *)

  datatype procedure_division =
    PROCEDURE_DIVISION_FORMAT_1_DECLARATIVES of using_clause *
                                                declaratives_section *
                                                sections *
                                                pos * pos
  | PROCEDURE_DIVISION_FORMAT_1_NO_DECLARATIVES of using_clause *
                                                   sections *
                                                   pos * pos
  | PROCEDURE_DIVISION_FORMAT_2 of using_clause *
                                   paragraphs * 
                                   pos * pos
  | EMPTY_PROCEDURE_DIVISION
     (* no procedure division allowed by our parser though mandatory in S/36 *)

(* END OF PROCEDURE DIVISION *)

(* PROGRAM STRUCTURE *)

  datatype data_division =
    DATA_DIVISION of file_section *
                     working_storage_section *
                     linkage_section
  | NO_DATA_DIVISION

  datatype environment_division =
    ENVIRONMENT_DIVISION of 
                            configuration_section *
                            input_output_section
  | NO_ENVIRONMENT_DIVISION

  type identification_division = unit

  datatype cobol_program =
    PROGRAM of identification_division *
               environment_division *
               data_division *
               procedure_division

  datatype test_cobol_programs =
    TEST_COBOL_PROGRAMS of test_cobol_programs * cobol_program
  | TEST_COBOL_PROGRAM of cobol_program

  datatype test_cobol =
    TEST of test_cobol_programs
  | COBOL_PROGRAM of cobol_program

end;



datatype cexpression =
  CE_AE of Cobol.arithmetic_expression * Cobol.pos * Cobol.pos
| CE_SINGLE_REL of Cobol.relational_operator * Cobol.arithmetic_expression *
                   Cobol.pos * Cobol.pos
| CE_REL of Cobol.arithmetic_expression *
            Cobol.is_not *
            Cobol.relational_operator *
            Cobol.arithmetic_expression * Cobol.pos * Cobol.pos
| CE_DC of Cobol.arithmetic_expression *
           Cobol.is_not *
           Cobol.data_class
| CE_SIGN of Cobol.arithmetic_expression *
             Cobol.is_not *
             Cobol.sign_specification
| CE_SWITCH of Cobol.pos * Cobol.pos
| CE_TRUE of Cobol.pos * Cobol.pos
| CE_FALSE of Cobol.pos * Cobol.pos
| CE_NOT of cexpression
| CE_AND of cexpression * cexpression
| CE_OR of cexpression * cexpression






             
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | PSEUDOTEXT of  (string)
 | USERDEFINEDWORD of  (string) | PICTURESTRING of  (string)
 | BOOLEANLITERAL of  (bool) | INTEGER of  (string)
 | DECIMALNUMBER of  (string) | NONNUMERICLITERAL of  (string)
 | identifier_or_literal_or_ddt of  (Cobol.identifier_or_literal_or_ddt)
 | ts2k of  (Cobol.ts2k)
 | annotation_statement of  (Cobol.annotation_statement)
 | data_description_annotation of  (Cobol.record_description_entry)
 | working_storage_section of  (Cobol.working_storage_section)
 | variable_length_tables_clause of  (Cobol.table_length_description*Cobol.ascending_descending_key_phrases*Cobol.indexed_by_phrase)
 | value_of_phrases of  (Cobol.value_of_phrases)
 | value_of_phrase of  (Cobol.value_of_phrase)
 | value_of_clause of  (Cobol.file_description_clause)
 | value_clause of  (Cobol.literal_ranges)
 | using_clause of  (Cobol.using_clause)
 | usage_specifier of  (Cobol.usage_specifier)
 | usage_clause of  (Cobol.usage_specifier)
 | top_bottom_specifications of  (Cobol.top_bottom_specifications)
 | top_bottom_specification of  (Cobol.top_bottom_specification)
 | top_bottom of  (Cobol.top_bottom_specification)
 | filler of  (Cobol.filler)
 | through_literal of  (Cobol.through_literal)
 | through_data_name of  (Cobol.through_data_name)
 | then_statements of  (Cobol.then_statements)
 | text_name of  (string) | system_name of  (string)
 | conditional_expression of  (Cobol.conditional_expression)
 | arithmetic_expression of  (Cobol.arithmetic_expression)
 | synchronized_clause of  (Cobol.left_right)
 | subtract_statement of  (Cobol.add_or_subtract_statement)
 | subscript_phrase of  (Cobol.subscript_phrase)
 | statements of  (Cobol.statements) | statement of  (Cobol.statement)
 | standard_or_omitted of  (Cobol.file_description_clause)
 | special_names_paragraph of  (Cobol.special_names_paragraph)
 | source_computer_paragraph of  (Cobol.source_computer_paragraph)
 | sort_description_entry of  (Cobol.sort_description_entry)
 | sort_description_clauses of  (Cobol.sort_description_clauses)
 | sort_description_clause of  (Cobol.sort_description_clause)
 | size_error_clauses of  (Cobol.size_error_clause)
 | size_error_clause of  (Cobol.size_error_clause)
 | sign_clause of  (Cobol.leading_trailing*Cobol.separate_character)
 | separate_character of  (Cobol.separate_character)
 | sentences of  (Cobol.sentences) | sentence of  (Cobol.sentence)
 | segment_number_opt of  (Cobol.segment_number_opt)
 | sections of  (Cobol.sections) | section_name of  (string)
 | rounded of  (Cobol.rounded)
 | renames_clause of  (Cobol.record_description_entry)
 | redefines_clause of  (Cobol.redefines_clause)
 | record_description_entry of  (Cobol.record_description_entry)
 | record_description_entries of  (Cobol.record_description_entries)
 | record_description_entries_opt of  (Cobol.record_description_entries_opt)
 | record_contains_clause of  (Cobol.record_contains_clause)
 | qualifications of  (Cobol.qualifications)
 | qualification of  (Cobol.qualification)
 | procedure_division of  (Cobol.procedure_division)
 | data_name_or_filler of  (Cobol.data_name_or_filler)
 | picture_clause of  (string)
 | paragraphs_and_sections of  (Cobol.paragraphs_and_sections)
 | paragraphs of  (Cobol.paragraphs) | paragraph_name of  (string)
 | paragraph of  (Cobol.paragraph)
 | occurs_clause of  (Cobol.table_length_description*Cobol.ascending_descending_key_phrases*Cobol.indexed_by_phrase)
 | object_computer_paragraph of  (Cobol.object_computer_paragraph)
 | multiply_statement of  (Cobol.multiply_statement)
 | move_statement of  (Cobol.move_statement)
 | mnemonic_name of  (string)
 | literal_ranges of  (Cobol.literal_ranges)
 | literal_range of  (Cobol.literal_range)
 | literal of  (Cobol.literal)
 | linkage_section of  (Cobol.linkage_section)
 | linage_clause of  (Cobol.file_description_clause)
 | level_number of  (string) | left_right of  (Cobol.left_right)
 | figurative_constant of  (Cobol.figurative_constant)
 | leading_trailing of  (Cobol.leading_trailing)
 | label_records_clause of  (Cobol.file_description_clause)
 | is_not of  (Cobol.is_not) | integer_range of  (Cobol.integer_range)
 | input_output_section of  (Cobol.input_output_section)
 | indicator_clause of  (string)
 | indexed_by_phrase of  (Cobol.indexed_by_phrase)
 | index_names of  (Cobol.index_names) | index_name of  (string)
 | imperative_statement of  (Cobol.statements)
 | if_statement of  (Cobol.if_statement)
 | identifiers of  (Cobol.identifiers)
 | identifier_roundeds of  (Cobol.identifier_roundeds)
 | identifier_rounded of  (Cobol.identifier_rounded)
 | identifier_or_literals of  (Cobol.identifier_or_literals)
 | identifier_or_literal of  (Cobol.identifier_or_literal)
 | identifier of  (Cobol.identifier)
 | identification_division of  (Cobol.identification_division)
 | from_environment of  (Cobol.from_environment)
 | footing_specification of  (Cobol.footing_specification)
 | fixed_length_tables_clause of  (Cobol.table_length_description*Cobol.ascending_descending_key_phrases*Cobol.indexed_by_phrase)
 | file_section of  (Cobol.file_section) | file_name of  (string)
 | file_description_paragraphs of  (Cobol.file_description_paragraphs)
 | file_description_paragraph of  (Cobol.file_description_paragraph)
 | file_description_entry of  (Cobol.file_description_entry)
 | file_description_clauses of  (Cobol.file_description_clauses)
 | file_description_clause of  (Cobol.file_description_clause)
 | file_and_sort_description_entry of  (Cobol.file_and_sort_description_entry)
 | special_register of  (Cobol.special_register)
 | environment_division of  (Cobol.environment_division)
 | else_statements of  (Cobol.else_statements)
 | divide_statement of  (Cobol.divide_statement)
 | sign_specification of  (Cobol.sign_specification)
 | data_class of  (Cobol.data_class)
 | relational_operator of  (Cobol.relational_operator)
 | declaratives_section of  (Cobol.declaratives_section)
 | decimal_clause of  (Cobol.decimal_clause)
 | date_day_time of  (Cobol.date_day_time)
 | data_records_clause of  (Cobol.data_records_clause)
 | data_name_or_literal of  (Cobol.data_name_or_literal)
 | special_names_clause of  (Cobol.special_names_clause)
 | special_names_clauses of  (Cobol.special_names_clauses)
 | data_name_or_integer of  (Cobol.data_name_or_integer)
 | data_names of  (Cobol.data_names) | data_name of  (string)
 | data_division of  (Cobol.data_division)
 | data_description_entry_134 of  (Cobol.record_description_entry)
 | data_description_entry of  (Cobol.record_description_entry)
 | data_description_clauses of  (Cobol.data_description_clauses)
 | data_description_clause of  (Cobol.data_description_clause)
 | currency_sign_clause of  (Cobol.currency_sign_clause)
 | configuration_section of  (Cobol.configuration_section)
 | compute_statement of  (Cobol.compute_statement)
 | code_set_clause of  (Cobol.file_description_clause)
 | characters_or_records of  (Cobol.characters_or_records)
 | character_string of  (string)
 | block_contains_clause of  (Cobol.file_description_clause)
 | ascending_descending_key_phrases of  (Cobol.ascending_descending_key_phrases)
 | ascending_descending_key_phrase of  (Cobol.ascending_descending_key_phrase)
 | ascending_descending of  (Cobol.ascending_descending)
 | expression of  (cexpression) | alphabet_name of  (string)
 | add_statement of  (Cobol.add_or_subtract_statement)
 | accept_statement of  (Cobol.accept_statement)
 | test_cobol of  (Cobol.test_cobol)
 | test_cobol_program of  (Cobol.cobol_program)
 | test_cobol_programs of  (Cobol.test_cobol_programs)
 | cobol_program of  (Cobol.cobol_program)
end
type svalue = MlyValue.svalue
(*TODO 13/01/1998 14:54. hojfeld.: mine erklringer:*)
type pos = unit
type arg = unit
datatype nonterm = hojfelds_NT of int
exception Hojfeld of string
(*og s: LrTable.NT |-> hojfelds_NT*)

(*TODO 13/01/1998 14:54. hojfeld.: mine erklringer slut*)
  
fun actions(i392:int, defaultPos:pos, stack:(unit (*LrTable.state*) * (svalue * pos * pos)) list,  
            ():arg
            ) =
case (i392, stack) of
  (0, _) => raise Hojfeld "0"   (*act0(stack)*)
| (1, _) => raise Hojfeld "1"   (*act1(stack)*)
| (2, _) => raise Hojfeld "2"   (*act2(stack)*)
| (3, _) => raise Hojfeld "3"   (*act3(stack)*)
| (4, _) => raise Hojfeld "4"   (*act4(stack)*)
| (5, _) => raise Hojfeld "5"   (*act5(stack)*)
| (6, _) => raise Hojfeld "6"   (*act6(stack)*)
| (7, _) => raise Hojfeld "7"   (*act7(stack)*)
| (8, _) => raise Hojfeld "8"   (*act8(stack,defaultPos)*)
| (9, _) => raise Hojfeld "9"   (*act9(stack)*)
| (10,(_,(_,USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 76,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right)
,rest671) end
| (11,(_,(MlyValue.USERDEFINEDWORD USERDEFINEDWORD,
USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => let val 
result=MlyValue.data_name((USERDEFINEDWORD))
 in (hojfelds_NT 96,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right)
,rest671) end
| (12,(_,(_,USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 98,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right)
,rest671) end

| (13,(_,(_,_,qdata_name1right))::_::(_,(_,USERDEFINEDWORD1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 98,(result,USERDEFINEDWORD1left,qdata_name1right),
rest671) end
| (14,(_,(_,_,qdata_name1right))::_::(_,(_,USERDEFINEDWORD1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 98,(result,USERDEFINEDWORD1left,qdata_name1right),
rest671) end
| (15,(_,(_,USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 389,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (16,(_,(MlyValue.USERDEFINEDWORD USERDEFINEDWORD,
USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => let val 
result=MlyValue.file_name((USERDEFINEDWORD))
 in (hojfelds_NT 200,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (17,(_,(MlyValue.USERDEFINEDWORD USERDEFINEDWORD,
USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => let val 
result=MlyValue.index_name((USERDEFINEDWORD))
 in (hojfelds_NT 244,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (18,(_,(MlyValue.USERDEFINEDWORD USERDEFINEDWORD,
USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => let val 
result=MlyValue.mnemonic_name((USERDEFINEDWORD))
 in (hojfelds_NT 309,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (19,(_,(_,USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 415,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (20,(_,(_,USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 291,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (21,(_,(_,USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 374,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (22,(_,(MlyValue.USERDEFINEDWORD USERDEFINEDWORD,
USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => let val 
result=MlyValue.text_name((USERDEFINEDWORD))
 in (hojfelds_NT 486,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (23,(_,(MlyValue.USERDEFINEDWORD USERDEFINEDWORD,
USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => let val 
result=MlyValue.paragraph_name((USERDEFINEDWORD))
 in (hojfelds_NT 357,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (24,(_,(MlyValue.INTEGER INTEGER,INTEGER1left,INTEGER1right))::
rest671) => let val result=MlyValue.paragraph_name((INTEGER))
 in (hojfelds_NT 357,(result,INTEGER1left,INTEGER1right),rest671) end
| (25,(_,(MlyValue.USERDEFINEDWORD USERDEFINEDWORD,
USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => let val 
result=MlyValue.section_name((USERDEFINEDWORD))
 in (hojfelds_NT 422,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (26,(_,(MlyValue.INTEGER INTEGER,INTEGER1left,INTEGER1right))::
rest671) => let val result=MlyValue.section_name((INTEGER))
 in (hojfelds_NT 422,(result,INTEGER1left,INTEGER1right),rest671) end
| (27,(_,(MlyValue.INTEGER INTEGER,INTEGER1left,INTEGER1right))::
rest671) => let val result=MlyValue.level_number((INTEGER))
 in (hojfelds_NT 290,(result,INTEGER1left,INTEGER1right),rest671) end
| (28,(_,(MlyValue.USERDEFINEDWORD USERDEFINEDWORD,
USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => let val 
result=MlyValue.system_name((USERDEFINEDWORD))
 in (hojfelds_NT 478,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (29,(_,(_,USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 74,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right)
,rest671) end
| (30,(_,(_,USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 286,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (31,(_,(_,ZERO1left,ZERO1right))::rest671) => let val result=
MlyValue.figurative_constant((Cobol.ZERO))
 in (hojfelds_NT 288,(result,ZERO1left,ZERO1right),rest671) end
| (32,(_,(_,ZEROS1left,ZEROS1right))::rest671) => let val result=
MlyValue.figurative_constant((Cobol.ZERO))
 in (hojfelds_NT 288,(result,ZEROS1left,ZEROS1right),rest671) end
| (33,(_,(_,ZEROES1left,ZEROES1right))::rest671) => let val result=
MlyValue.figurative_constant((Cobol.ZERO))
 in (hojfelds_NT 288,(result,ZEROES1left,ZEROES1right),rest671) end
| (34,(_,(_,SPACE1left,SPACE1right))::rest671) => let val result=
MlyValue.figurative_constant((Cobol.SPACE))
 in (hojfelds_NT 288,(result,SPACE1left,SPACE1right),rest671) end
| (35,(_,(_,SPACES1left,SPACES1right))::rest671) => let val result=
MlyValue.figurative_constant((Cobol.SPACE))
 in (hojfelds_NT 288,(result,SPACES1left,SPACES1right),rest671) end
| (36,(_,(_,HIGHVALUE1left,HIGHVALUE1right))::rest671) => let val 
result=MlyValue.figurative_constant((Cobol.HIGHVALUE))
 in (hojfelds_NT 288,(result,HIGHVALUE1left,HIGHVALUE1right),rest671)
 end
| (37,(_,(_,HIGHVALUES1left,HIGHVALUES1right))::rest671) => let val 
result=MlyValue.figurative_constant((Cobol.HIGHVALUE))
 in (hojfelds_NT 288,(result,HIGHVALUES1left,HIGHVALUES1right),rest671)
 end
| (38,(_,(_,LOWVALUE1left,LOWVALUE1right))::rest671) => let val result
=MlyValue.figurative_constant((Cobol.LOWVALUE))
 in (hojfelds_NT 288,(result,LOWVALUE1left,LOWVALUE1right),rest671) end
| (39,(_,(_,LOWVALUES1left,LOWVALUES1right))::rest671) => let val 
result=MlyValue.figurative_constant((Cobol.LOWVALUE))
 in (hojfelds_NT 288,(result,LOWVALUES1left,LOWVALUES1right),rest671)
 end
| (40,(_,(_,QUOTE1left,QUOTE1right))::rest671) => let val result=
MlyValue.figurative_constant((Cobol.QUOTE))
 in (hojfelds_NT 288,(result,QUOTE1left,QUOTE1right),rest671) end
| (41,(_,(_,QUOTES1left,QUOTES1right))::rest671) => let val result=
MlyValue.figurative_constant((Cobol.QUOTE))
 in (hojfelds_NT 288,(result,QUOTES1left,QUOTES1right),rest671) end
| (42,(_,(_,_,ZERO1right))::(_,(_,ALL1left,_))::rest671) => let val 
result=MlyValue.figurative_constant((Cobol.ALL_ZERO))
 in (hojfelds_NT 288,(result,ALL1left,ZERO1right),rest671) end
| (43,(_,(_,_,ZEROS1right))::(_,(_,ALL1left,_))::rest671) => let val 
result=MlyValue.figurative_constant((Cobol.ALL_ZERO))
 in (hojfelds_NT 288,(result,ALL1left,ZEROS1right),rest671) end
| (44,(_,(_,_,ZEROES1right))::(_,(_,ALL1left,_))::rest671) => let val 
result=MlyValue.figurative_constant((Cobol.ALL_ZERO))
 in (hojfelds_NT 288,(result,ALL1left,ZEROES1right),rest671) end
| (45,(_,(_,_,SPACE1right))::(_,(_,ALL1left,_))::rest671) => let val 
result=MlyValue.figurative_constant((Cobol.ALL_SPACE))
 in (hojfelds_NT 288,(result,ALL1left,SPACE1right),rest671) end
| (46,(_,(_,_,SPACES1right))::(_,(_,ALL1left,_))::rest671) => let val 
result=MlyValue.figurative_constant((Cobol.ALL_SPACE))
 in (hojfelds_NT 288,(result,ALL1left,SPACES1right),rest671) end
| (47,(_,(_,_,HIGHVALUE1right))::(_,(_,ALL1left,_))::rest671) => let 
val result=MlyValue.figurative_constant((Cobol.ALL_HIGHVALUE))
 in (hojfelds_NT 288,(result,ALL1left,HIGHVALUE1right),rest671) end
| (48,(_,(_,_,HIGHVALUES1right))::(_,(_,ALL1left,_))::rest671) => let 
val result=MlyValue.figurative_constant((Cobol.ALL_HIGHVALUE))
 in (hojfelds_NT 288,(result,ALL1left,HIGHVALUES1right),rest671) end
| (49,(_,(_,_,LOWVALUE1right))::(_,(_,ALL1left,_))::rest671) => let 
val result=MlyValue.figurative_constant((Cobol.ALL_LOWVALUE))
 in (hojfelds_NT 288,(result,ALL1left,LOWVALUE1right),rest671) end
| (50,(_,(_,_,LOWVALUES1right))::(_,(_,ALL1left,_))::rest671) => let 
val result=MlyValue.figurative_constant((Cobol.ALL_LOWVALUE))
 in (hojfelds_NT 288,(result,ALL1left,LOWVALUES1right),rest671) end
| (51,(_,(_,_,QUOTE1right))::(_,(_,ALL1left,_))::rest671) => let val 
result=MlyValue.figurative_constant((Cobol.ALL_QUOTE))
 in (hojfelds_NT 288,(result,ALL1left,QUOTE1right),rest671) end
| (52,(_,(_,_,QUOTES1right))::(_,(_,ALL1left,_))::rest671) => let val 
result=MlyValue.figurative_constant((Cobol.ALL_QUOTE))
 in (hojfelds_NT 288,(result,ALL1left,QUOTES1right),rest671) end
| (53,(_,(MlyValue.NONNUMERICLITERAL NONNUMERICLITERAL,_,
NONNUMERICLITERAL1right))::(_,(_,ALL1left,_))::rest671) => let val 
result=MlyValue.figurative_constant((
Cobol.ALL_NONNUMERICLITERAL(NONNUMERICLITERAL)))
 in (hojfelds_NT 288,(result,ALL1left,NONNUMERICLITERAL1right),rest671)
 end
| (54,(_,(_,LINAGECOUNTER1left,LINAGECOUNTER1right))::rest671) => let 
val result=MlyValue.special_register((Cobol.LINAGECOUNTER))
 in (hojfelds_NT 181,(result,LINAGECOUNTER1left,LINAGECOUNTER1right),
rest671) end
| (55,(_,(_,DEBUGITEM1left,DEBUGITEM1right))::rest671) => let val 
result=MlyValue.special_register((Cobol.DEBUGITEM))
 in (hojfelds_NT 181,(result,DEBUGITEM1left,DEBUGITEM1right),rest671)
 end
| (56,(_,(_,PERIOD1left,PERIOD1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 361,(result,PERIOD1left,PERIOD1right),rest671) end
| (57,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 361,(result,defaultPos,defaultPos),rest671) end
| (58,(_,(_,data_name1left,data_name1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 100,(result,data_name1left,data_name1right),rest671)
 end
| (59,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 100,(result,defaultPos,defaultPos),rest671) end
| (60,(_,(_,procedure_name1left,procedure_name1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 369,(result,procedure_name1left,procedure_name1right),
rest671) end
| (61,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 369,(result,defaultPos,defaultPos),rest671) end
| (62,(_,(_,routine_name1left,routine_name1right))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 183,(result,routine_name1left,routine_name1right),
rest671) end
| (63,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 183,(result,defaultPos,defaultPos),rest671) end
| (64,(_,(MlyValue.INTEGER INTEGER,INTEGER1left,INTEGER1right))::
rest671) => let val result=MlyValue.segment_number_opt((
Cobol.SEGMENT_NUMBER(INTEGER)))
 in (hojfelds_NT 428,(result,INTEGER1left,INTEGER1right),rest671) end
| (65,rest671) => let val result=MlyValue.segment_number_opt((
Cobol.NOSEGMENT_NUMBER))
 in (hojfelds_NT 428,(result,defaultPos,defaultPos),rest671) end
| (66,(_,(MlyValue.data_name data_name,_,data_name1right))::(_,(
MlyValue.data_names data_names,data_names1left,_))::rest671) => let 
val result=MlyValue.data_names((
Cobol.DATA_NAMES(data_names,data_name )))
 in (hojfelds_NT 97,(result,data_names1left,data_name1right),rest671)
 end
| (67,(_,(MlyValue.data_name data_name,data_name1left,data_name1right)
)::rest671) => let val result=MlyValue.data_names((
Cobol.ONE_DATA_NAME(data_name)))
 in (hojfelds_NT 97,(result,data_name1left,data_name1right),rest671)
 end
| (68,(_,(_,_,qdata_name1right))::(_,(_,qdata_names1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 99,(result,qdata_names1left,qdata_name1right),rest671)
 end
| (69,(_,(_,qdata_name1left,qdata_name1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 99,(result,qdata_name1left,qdata_name1right),rest671)
 end
| (70,(_,(_,_,file_name1right))::(_,(_,file_names1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 203,(result,file_names1left,file_name1right),rest671)
 end
| (71,(_,(_,file_name1left,file_name1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 203,(result,file_name1left,file_name1right),rest671)
 end
| (72,(_,(MlyValue.index_name index_name,_,index_name1right))::(_,(
MlyValue.index_names index_names,index_names1left,_))::rest671) => 
let val result=MlyValue.index_names((
Cobol.SEVERAL_INDEX_NAMES(index_names,index_name)))
 in (hojfelds_NT 245,(result,index_names1left,index_name1right),rest671
) end
| (73,(_,(MlyValue.index_name index_name,index_name1left,
index_name1right))::rest671) => let val result=MlyValue.index_names((
Cobol.ONE_INDEX_NAME(index_name)))
 in (hojfelds_NT 245,(result,index_name1left,index_name1right),rest671)
 end
| (74,(_,(_,_,procedure_name1right))::(_,(_,procedure_names1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 370,(result,procedure_names1left,procedure_name1right)
,rest671) end
| (75,(_,(_,procedure_name1left,procedure_name1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 370,(result,procedure_name1left,procedure_name1right),
rest671) end
| (76,(_,(_,USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 106,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (77,(_,(_,USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 368,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right
),rest671) end
| (78,(_,(_,INTEGER1left,INTEGER1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 368,(result,INTEGER1left,INTEGER1right),rest671) end
| (79,(_,(_,SYSTEMCONSOLE1left,SYSTEMCONSOLE1right))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 154,(result,SYSTEMCONSOLE1left,SYSTEMCONSOLE1right),
rest671) end
| (80,(_,(_,REQUESTOR1left,REQUESTOR1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 154,(result,REQUESTOR1left,REQUESTOR1right),rest671)
 end
| (81,(_,(_,LOCALDATA1left,LOCALDATA1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 154,(result,LOCALDATA1left,LOCALDATA1right),rest671)
 end
| (82,(_,(_,ATTRIBUTEDATA1left,ATTRIBUTEDATA1right))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 154,(result,ATTRIBUTEDATA1left,ATTRIBUTEDATA1right),
rest671) end
| (83,(_,(_,C011left,C011right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 154,(result,C011left,C011right),rest671) end
| (84,(_,(_,CSP1left,CSP1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 154,(result,CSP1left,CSP1right),rest671) end
| (85,(_,(_,SYSTEMSHUTDOWN1left,SYSTEMSHUTDOWN1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 155,(result,SYSTEMSHUTDOWN1left,SYSTEMSHUTDOWN1right),
rest671) end
| (86,(_,(_,UPSI01left,UPSI01right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 155,(result,UPSI01left,UPSI01right),rest671) end
| (87,(_,(_,UPSI11left,UPSI11right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 155,(result,UPSI11left,UPSI11right),rest671) end
| (88,(_,(_,UPSI21left,UPSI21right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 155,(result,UPSI21left,UPSI21right),rest671) end
| (89,(_,(_,UPSI31left,UPSI31right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 155,(result,UPSI31left,UPSI31right),rest671) end
| (90,(_,(_,UPSI41left,UPSI41right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 155,(result,UPSI41left,UPSI41right),rest671) end
| (91,(_,(_,UPSI51left,UPSI51right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 155,(result,UPSI51left,UPSI51right),rest671) end
| (92,(_,(_,UPSI61left,UPSI61right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 155,(result,UPSI61left,UPSI61right),rest671) end
| (93,(_,(_,UPSI71left,UPSI71right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 155,(result,UPSI71left,UPSI71right),rest671) end
| (94,(_,(_,USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 47,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right)
,rest671) end
| (95,(_,(_,ADVANCING1left,ADVANCING1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 11,(result,ADVANCING1left,ADVANCING1right),rest671)
 end
| (96,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 11,(result,defaultPos,defaultPos),rest671) end
| (97,(_,(_,ALL1left,ALL1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 15,(result,ALL1left,ALL1right),rest671) end
| (98,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 15,(result,defaultPos,defaultPos),rest671) end
| (99,(_,(_,ARE1left,ARE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 38,(result,ARE1left,ARE1right),rest671) end
| (100,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 38,(result,defaultPos,defaultPos),rest671) end
| (101,(_,(_,AREA1left,AREA1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 39,(result,AREA1left,AREA1right),rest671) end
| (102,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 39,(result,defaultPos,defaultPos),rest671) end
| (103,(_,(_,AREAS1left,AREAS1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 41,(result,AREAS1left,AREAS1right),rest671) end
| (104,(_,(_,AREA1left,AREA1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 41,(result,AREA1left,AREA1right),rest671) end
| (105,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 41,(result,defaultPos,defaultPos),rest671) end
| (106,(_,(_,_,for_opt1right))::(_,(_,area1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 40,(result,area1left,for_opt1right),rest671) end
| (107,(_,(_,AT1left,AT1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 48,(result,AT1left,AT1right),rest671) end
| (108,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 48,(result,defaultPos,defaultPos),rest671) end
| (109,(_,(_,BY1left,BY1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 54,(result,BY1left,BY1right),rest671) end
| (110,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 54,(result,defaultPos,defaultPos),rest671) end
| (111,(_,(_,CHARACTER1left,CHARACTER1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 59,(result,CHARACTER1left,CHARACTER1right),rest671)
 end
| (112,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 59,(result,defaultPos,defaultPos),rest671) end
| (113,(_,(_,CHARACTERS1left,CHARACTERS1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 61,(result,CHARACTERS1left,CHARACTERS1right),rest671)
 end
| (114,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 61,(result,defaultPos,defaultPos),rest671) end
| (115,(_,(_,COLLATING1left,COLLATING1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 66,(result,COLLATING1left,COLLATING1right),rest671)
 end
| (116,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 66,(result,defaultPos,defaultPos),rest671) end
| (117,(_,(_,CONTAINS1left,CONTAINS1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 78,(result,CONTAINS1left,CONTAINS1right),rest671) end
| (118,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 78,(result,defaultPos,defaultPos),rest671) end
| (119,(_,(_,EVERY1left,EVERY1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 159,(result,EVERY1left,EVERY1right),rest671) end
| (120,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 159,(result,defaultPos,defaultPos),rest671) end
| (121,(_,(_,FILE1left,FILE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 166,(result,FILE1left,FILE1right),rest671) end
| (122,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 166,(result,defaultPos,defaultPos),rest671) end
| (123,(_,(_,FILLER1left,FILLER1right))::rest671) => let val result=
MlyValue.filler((Cobol.FILLER))
 in (hojfelds_NT 496,(result,FILLER1left,FILLER1right),rest671) end
| (124,rest671) => let val result=MlyValue.filler((Cobol.NO_FILLER))
 in (hojfelds_NT 496,(result,defaultPos,defaultPos),rest671) end
| (125,(_,(_,FOR1left,FOR1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 213,(result,FOR1left,FOR1right),rest671) end
| (126,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 213,(result,defaultPos,defaultPos),rest671) end
| (127,(_,(_,_,REMOVAL1right))::(_,(_,for_opt1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 214,(result,for_opt1left,REMOVAL1right),rest671) end
| (128,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 214,(result,defaultPos,defaultPos),rest671) end
| (129,(_,(_,INITIAL1left,INITIAL1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 253,(result,INITIAL1left,INITIAL1right),rest671) end
| (130,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 253,(result,defaultPos,defaultPos),rest671) end
| (131,(_,(_,IN1left,IN1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 242,(result,IN1left,IN1right),rest671) end
| (132,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 242,(result,defaultPos,defaultPos),rest671) end
| (133,(_,(_,IS1left,IS1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 278,(result,IS1left,IS1right),rest671) end
| (134,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 278,(result,defaultPos,defaultPos),rest671) end
| (135,(_,(_,_,NOT1right))::(_,(_,IS1left,_))::rest671) => let val 
result=MlyValue.is_not((Cobol.IS_NOT_NOT))
 in (hojfelds_NT 279,(result,IS1left,NOT1right),rest671) end
| (136,(_,(_,IS1left,IS1right))::rest671) => let val result=
MlyValue.is_not((Cobol.IS_NOT_IS))
 in (hojfelds_NT 279,(result,IS1left,IS1right),rest671) end
| (137,(_,(_,NOT1left,NOT1right))::rest671) => let val result=
MlyValue.is_not((Cobol.IS_NOT_NOT))
 in (hojfelds_NT 279,(result,NOT1left,NOT1right),rest671) end
| (138,rest671) => let val result=MlyValue.is_not((Cobol.IS_NOT_IS))
 in (hojfelds_NT 279,(result,defaultPos,defaultPos),rest671) end
| (139,(_,(_,KEY1left,KEY1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 282,(result,KEY1left,KEY1right),rest671) end
| (140,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 282,(result,defaultPos,defaultPos),rest671) end
| (141,(_,(_,LINE1left,LINE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 294,(result,LINE1left,LINE1right),rest671) end
| (142,(_,(_,lines1left,lines1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 294,(result,lines1left,lines1right),rest671) end
| (143,(_,(_,LINES1left,LINES1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 295,(result,LINES1left,LINES1right),rest671) end
| (144,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 295,(result,defaultPos,defaultPos),rest671) end
| (145,(_,(_,_,is1right))::(_,(_,MODE1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 313,(result,MODE1left,is1right),rest671) end
| (146,(_,(_,is1left,is1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 313,(result,is1left,is1right),rest671) end
| (147,(_,(_,NEXT1left,NEXT1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 148,(result,NEXT1left,NEXT1right),rest671) end
| (148,(_,(_,FIRST1left,FIRST1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 148,(result,FIRST1left,FIRST1right),rest671) end
| (149,(_,(_,LAST1left,LAST1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 148,(result,LAST1left,LAST1right),rest671) end
| (150,(_,(_,PRIOR1left,PRIOR1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 148,(result,PRIOR1left,PRIOR1right),rest671) end
| (151,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 148,(result,defaultPos,defaultPos),rest671) end
| (152,(_,(_,OF1left,OF1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 331,(result,OF1left,OF1right),rest671) end
| (153,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 331,(result,defaultPos,defaultPos),rest671) end
| (154,(_,(_,ON1left,ON1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 335,(result,ON1left,ON1right),rest671) end
| (155,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 335,(result,defaultPos,defaultPos),rest671) end
| (156,(_,(_,OPTIONAL1left,OPTIONAL1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 348,(result,OPTIONAL1left,OPTIONAL1right),rest671) end
| (157,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 348,(result,defaultPos,defaultPos),rest671) end
| (158,(_,(_,_,is1right))::(_,(_,ORGANIZATION1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 352,(result,ORGANIZATION1left,is1right),rest671) end
| (159,(_,(_,_,TO1right))::(_,(_,PROCEED1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 371,(result,PROCEED1left,TO1right),rest671) end
| (160,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 371,(result,defaultPos,defaultPos),rest671) end
| (161,(_,(_,_,COLLATING1right))::(_,(_,PROGRAM1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 373,(result,PROGRAM1left,COLLATING1right),rest671) end
| (162,(_,(_,PROGRAM1left,PROGRAM1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 373,(result,PROGRAM1left,PROGRAM1right),rest671) end
| (163,(_,(_,COLLATING1left,COLLATING1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 373,(result,COLLATING1left,COLLATING1right),rest671)
 end
| (164,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 373,(result,defaultPos,defaultPos),rest671) end
| (165,(_,(_,RECORD1left,RECORD1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 378,(result,RECORD1left,RECORD1right),rest671) end
| (166,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 378,(result,defaultPos,defaultPos),rest671) end
| (167,(_,(_,RIGHT1left,RIGHT1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 413,(result,RIGHT1left,RIGHT1right),rest671) end
| (168,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 413,(result,defaultPos,defaultPos),rest671) end
| (169,(_,(_,ROUNDED1left,ROUNDED1right))::rest671) => let val result=
MlyValue.rounded((Cobol.ROUNDED))
 in (hojfelds_NT 414,(result,ROUNDED1left,ROUNDED1right),rest671) end
| (170,rest671) => let val result=MlyValue.rounded((Cobol.NOT_ROUNDED)
)
 in (hojfelds_NT 414,(result,defaultPos,defaultPos),rest671) end
| (171,(_,(_,_,character1right))::(_,(_,SEPARATE1left,_))::rest671)
 => let val result=MlyValue.separate_character((Cobol.SEPARATE))
 in (hojfelds_NT 431,(result,SEPARATE1left,character1right),rest671)
 end
| (172,rest671) => let val result=MlyValue.separate_character((
Cobol.NO_SEPARATE))
 in (hojfelds_NT 431,(result,defaultPos,defaultPos),rest671) end
| (173,(_,(_,_,IS1right))::(_,(_,SIGN1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 437,(result,SIGN1left,IS1right),rest671) end
| (174,(_,(_,IS1left,IS1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 437,(result,IS1left,IS1right),rest671) end
| (175,(_,(_,_,is1right))::(_,(_,SIGN1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 438,(result,SIGN1left,is1right),rest671) end
| (176,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 438,(result,defaultPos,defaultPos),rest671) end
| (177,(_,(_,SIZE1left,SIZE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 440,(result,SIZE1left,SIZE1right),rest671) end
| (178,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 440,(result,defaultPos,defaultPos),rest671) end
| (179,(_,(_,STANDARD1left,STANDARD1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 455,(result,STANDARD1left,STANDARD1right),rest671) end
| (180,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 455,(result,defaultPos,defaultPos),rest671) end
| (181,(_,(_,_,IS1right))::(_,(_,STATUS1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 464,(result,STATUS1left,IS1right),rest671) end
| (182,(_,(_,IS1left,IS1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 464,(result,IS1left,IS1right),rest671) end
| (183,(_,(_,_,CONTAINS1right))::(_,(_,TAPE1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 483,(result,TAPE1left,CONTAINS1right),rest671) end
| (184,(_,(_,TAPE1left,TAPE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 483,(result,TAPE1left,TAPE1right),rest671) end
| (185,(_,(_,CONTAINS1left,CONTAINS1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 483,(result,CONTAINS1left,CONTAINS1right),rest671) end
| (186,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 483,(result,defaultPos,defaultPos),rest671) end
| (187,(_,(_,THAN1left,THAN1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 487,(result,THAN1left,THAN1right),rest671) end
| (188,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 487,(result,defaultPos,defaultPos),rest671) end
| (189,(_,(_,THEN1left,THEN1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 488,(result,THEN1left,THEN1right),rest671) end
| (190,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 488,(result,defaultPos,defaultPos),rest671) end
| (191,(_,(_,TIMES1left,TIMES1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 497,(result,TIMES1left,TIMES1right),rest671) end
| (192,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 497,(result,defaultPos,defaultPos),rest671) end
| (193,(_,(_,TO1left,TO1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 499,(result,TO1left,TO1right),rest671) end
| (194,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 499,(result,defaultPos,defaultPos),rest671) end
| (195,(_,(_,_,is1right))::(_,(_,USAGE1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 508,(result,USAGE1left,is1right),rest671) end
| (196,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 508,(result,defaultPos,defaultPos),rest671) end
| (197,(_,(_,WHEN1left,WHEN1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 524,(result,WHEN1left,WHEN1right),rest671) end
| (198,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 524,(result,defaultPos,defaultPos),rest671) end
| (199,(_,(_,WITH1left,WITH1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 528,(result,WITH1left,WITH1right),rest671) end
| (200,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 528,(result,defaultPos,defaultPos),rest671) end
| (201,(_,(_,CORRESPONDING1left,CORRESPONDING1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 82,(result,CORRESPONDING1left,CORRESPONDING1right),
rest671) end
| (202,(_,(_,CORR1left,CORR1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 82,(result,CORR1left,CORR1right),rest671) end
| (203,(_,(_,JUSTIFIED1left,JUSTIFIED1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 280,(result,JUSTIFIED1left,JUSTIFIED1right),rest671)
 end
| (204,(_,(_,JUST1left,JUST1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 280,(result,JUST1left,JUST1right),rest671) end
| (205,(_,(_,PICTURE1left,PICTURE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 362,(result,PICTURE1left,PICTURE1right),rest671) end
| (206,(_,(_,PIC1left,PIC1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 362,(result,PIC1left,PIC1right),rest671) end
| (207,(_,(_,SYNCHRONIZED1left,SYNCHRONIZED1right))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 474,(result,SYNCHRONIZED1left,SYNCHRONIZED1right),
rest671) end
| (208,(_,(_,SYNC1left,SYNC1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 474,(result,SYNC1left,SYNC1right),rest671) end
| (209,(_,(_,THROUGH1left,THROUGH1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 491,(result,THROUGH1left,THROUGH1right),rest671) end
| (210,(_,(_,THRU1left,THRU1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 491,(result,THRU1left,THRU1right),rest671) end
| (211,(_,(_,ENDADD1left,ENDADD1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 131,(result,ENDADD1left,ENDADD1right),rest671) end
| (212,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 131,(result,defaultPos,defaultPos),rest671) end
| (213,(_,(_,ENDCALL1left,ENDCALL1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 132,(result,ENDCALL1left,ENDCALL1right),rest671) end
| (214,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 132,(result,defaultPos,defaultPos),rest671) end
| (215,(_,(_,ENDCOMPUTE1left,ENDCOMPUTE1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 133,(result,ENDCOMPUTE1left,ENDCOMPUTE1right),rest671)
 end
| (216,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 133,(result,defaultPos,defaultPos),rest671) end
| (217,(_,(_,ENDDELETE1left,ENDDELETE1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 134,(result,ENDDELETE1left,ENDDELETE1right),rest671)
 end
| (218,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 134,(result,defaultPos,defaultPos),rest671) end
| (219,(_,(_,ENDDIVIDE1left,ENDDIVIDE1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 135,(result,ENDDIVIDE1left,ENDDIVIDE1right),rest671)
 end
| (220,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 135,(result,defaultPos,defaultPos),rest671) end
| (221,(_,(_,ENDIF1left,ENDIF1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 138,(result,ENDIF1left,ENDIF1right),rest671) end
| (222,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 138,(result,defaultPos,defaultPos),rest671) end
| (223,(_,(_,ENDMULTIPLY1left,ENDMULTIPLY1right))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 139,(result,ENDMULTIPLY1left,ENDMULTIPLY1right),
rest671) end
| (224,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 139,(result,defaultPos,defaultPos),rest671) end
| (225,(_,(_,ENDREAD1left,ENDREAD1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 142,(result,ENDREAD1left,ENDREAD1right),rest671) end
| (226,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 142,(result,defaultPos,defaultPos),rest671) end
| (227,(_,(_,ENDRETURN1left,ENDRETURN1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 143,(result,ENDRETURN1left,ENDRETURN1right),rest671)
 end
| (228,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 143,(result,defaultPos,defaultPos),rest671) end
| (229,(_,(_,ENDREWRITE1left,ENDREWRITE1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 144,(result,ENDREWRITE1left,ENDREWRITE1right),rest671)
 end
| (230,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 144,(result,defaultPos,defaultPos),rest671) end
| (231,(_,(_,ENDSEARCH1left,ENDSEARCH1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 145,(result,ENDSEARCH1left,ENDSEARCH1right),rest671)
 end
| (232,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 145,(result,defaultPos,defaultPos),rest671) end
| (233,(_,(_,ENDSTART1left,ENDSTART1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 182,(result,ENDSTART1left,ENDSTART1right),rest671) end
| (234,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 182,(result,defaultPos,defaultPos),rest671) end
| (235,(_,(_,ENDSTRING1left,ENDSTRING1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 146,(result,ENDSTRING1left,ENDSTRING1right),rest671)
 end
| (236,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 146,(result,defaultPos,defaultPos),rest671) end
| (237,(_,(_,ENDSUBTRACT1left,ENDSUBTRACT1right))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 147,(result,ENDSUBTRACT1left,ENDSUBTRACT1right),
rest671) end
| (238,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 147,(result,defaultPos,defaultPos),rest671) end
| (239,(_,(_,ENDUNSTRING1left,ENDUNSTRING1right))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 149,(result,ENDUNSTRING1left,ENDUNSTRING1right),
rest671) end
| (240,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 149,(result,defaultPos,defaultPos),rest671) end
| (241,(_,(_,ENDWRITE1left,ENDWRITE1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 150,(result,ENDWRITE1left,ENDWRITE1right),rest671) end
| (242,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 150,(result,defaultPos,defaultPos),rest671) end
| (243,(_,(MlyValue.identifier identifier,_,identifier1right))::(_,(
MlyValue.identifiers identifiers,identifiers1left,_))::rest671) => 
let val result=MlyValue.identifiers((
Cobol.SEVERAL_IDENTIFIERS(identifiers,identifier)))
 in (hojfelds_NT 239,(result,identifiers1left,identifier1right),rest671
) end
| (244,(_,(MlyValue.identifier identifier,identifier1left,
identifier1right))::rest671) => let val result=MlyValue.identifiers((
Cobol.ONE_IDENTIFIER(identifier)))
 in (hojfelds_NT 239,(result,identifier1left,identifier1right),rest671)
 end
| (245,(_,(MlyValue.subscript_phrase subscript_phrase,_,
subscript_phraseright as subscript_phrase1right))::(_,(
MlyValue.qualifications qualifications,_,_))::(_,(
MlyValue.USERDEFINEDWORD USERDEFINEDWORD,USERDEFINEDWORDleft as 
USERDEFINEDWORD1left,_))::rest671) => let val result=
MlyValue.identifier((
Cobol.QUALIFIED_IDENTIFIER(USERDEFINEDWORD,
                                       qualifications,
                                       subscript_phrase,
                                       USERDEFINEDWORDleft,
                                       subscript_phraseright)
))
 in (hojfelds_NT 228,(result,USERDEFINEDWORD1left,
subscript_phrase1right),rest671) end
| (246,(_,(MlyValue.special_register special_register,
special_registerleft as special_register1left,special_registerright
 as special_register1right))::rest671) => let val result=
MlyValue.identifier((
Cobol.SPECIAL_REGISTER(special_register,special_registerleft,special_registerright)
))
 in (hojfelds_NT 228,(result,special_register1left,
special_register1right),rest671) end
| (247,(_,(MlyValue.qualification qualification,_,qualification1right)
)::(_,(MlyValue.qualifications qualifications,qualifications1left,_))
::rest671) => let val result=MlyValue.qualifications((
Cobol.SEVERAL_QUALIFICATIONS(qualifications,qualification)))
 in (hojfelds_NT 376,(result,qualifications1left,qualification1right),
rest671) end
| (248,rest671) => let val result=MlyValue.qualifications((
Cobol.NO_QUALIFICATIONS))
 in (hojfelds_NT 376,(result,defaultPos,defaultPos),rest671) end
| (249,(_,(MlyValue.USERDEFINEDWORD USERDEFINEDWORD,_,
USERDEFINEDWORD1right))::(_,(_,of_in1left,_))::rest671) => let val 
result=MlyValue.qualification((USERDEFINEDWORD))
 in (hojfelds_NT 375,(result,of_in1left,USERDEFINEDWORD1right),rest671)
 end
| (250,(_,(_,OF1left,OF1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 330,(result,OF1left,OF1right),rest671) end
| (251,(_,(_,IN1left,IN1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 330,(result,IN1left,IN1right),rest671) end
| (252,(_,(_,_,RPAR1right))::_::(_,(_,LPAR1left,_))::rest671) => let 
val result=MlyValue.subscript_phrase(())
 in (hojfelds_NT 471,(result,LPAR1left,RPAR1right),rest671) end
| (253,rest671) => let val result=MlyValue.subscript_phrase(())
 in (hojfelds_NT 471,(result,defaultPos,defaultPos),rest671) end
| (254,(_,(_,_,subscript1right))::(_,(_,subscripts1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 472,(result,subscripts1left,subscript1right),rest671)
 end
| (255,(_,(_,subscript1left,subscript1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 472,(result,subscript1left,subscript1right),rest671)
 end
| (256,(_,(_,INTEGER1left,INTEGER1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 470,(result,INTEGER1left,INTEGER1right),rest671) end
| (257,(_,(_,_,offset1right))::(_,(_,data_or_index_name1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 470,(result,data_or_index_name1left,offset1right),
rest671) end
| (258,(_,(_,_,INTEGER1right))::(_,(_,PLUSSYMBOL1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 334,(result,PLUSSYMBOL1left,INTEGER1right),rest671)
 end
| (259,(_,(_,_,INTEGER1right))::(_,(_,MINUSSYMBOL1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 334,(result,MINUSSYMBOL1left,INTEGER1right),rest671)
 end
| (260,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 334,(result,defaultPos,defaultPos),rest671) end
| (261,(_,(MlyValue.data_name data_name,data_name1left,data_name1right
))::rest671) => let val result=MlyValue.data_name_or_literal((
Cobol.DATA_NAME_OR_LITERAL_IS_DATA_NAME(data_name)))
 in (hojfelds_NT 105,(result,data_name1left,data_name1right),rest671)
 end
| (262,(_,(MlyValue.literal literal,literal1left,literal1right))::
rest671) => let val result=MlyValue.data_name_or_literal((
Cobol.DATA_NAME_OR_LITERAL_IS_LITERAL(literal)))
 in (hojfelds_NT 105,(result,literal1left,literal1right),rest671) end
| (263,(_,(MlyValue.data_name data_name,data_name1left,data_name1right
))::rest671) => let val result=MlyValue.data_name_or_integer((
Cobol.DATA_NAME_OR_INTEGER_IS_DATA_NAME(data_name)))
 in (hojfelds_NT 101,(result,data_name1left,data_name1right),rest671)
 end
| (264,(_,(MlyValue.INTEGER INTEGER,INTEGER1left,INTEGER1right))::
rest671) => let val result=MlyValue.data_name_or_integer((
Cobol.DATA_NAME_OR_INTEGER_IS_INTEGER(INTEGER)))
 in (hojfelds_NT 101,(result,INTEGER1left,INTEGER1right),rest671) end
| (265,(_,(MlyValue.data_name data_name,data_name1left,data_name1right
))::rest671) => let val result=MlyValue.data_name_or_filler((
Cobol.DATA_NAME_OR_FILLER_IS_DATA_NAME(data_name)))
 in (hojfelds_NT 366,(result,data_name1left,data_name1right),rest671)
 end
| (266,(_,(MlyValue.filler filler,filler1left,filler1right))::rest671)
 => let val result=MlyValue.data_name_or_filler((
Cobol.DATA_NAME_OR_FILLER_IS_FILLER(filler)))
 in (hojfelds_NT 366,(result,filler1left,filler1right),rest671) end
| (267,(_,(MlyValue.identifier_or_literal identifier_or_literal,_,
identifier_or_literal1right))::(_,(MlyValue.identifier_or_literals 
identifier_or_literals,identifier_or_literals1left,_))::rest671) => 
let val result=MlyValue.identifier_or_literals((
Cobol.SEVERAL_IDENTIFIER_OR_LITERALS(identifier_or_literals,
                                          identifier_or_literal)
))
 in (hojfelds_NT 234,(result,identifier_or_literals1left,
identifier_or_literal1right),rest671) end
| (268,(_,(MlyValue.identifier_or_literal identifier_or_literal,
identifier_or_literal1left,identifier_or_literal1right))::rest671) => 
let val result=MlyValue.identifier_or_literals((
Cobol.ONE_IDENTIFIER_OR_LITERAL(identifier_or_literal)))
 in (hojfelds_NT 234,(result,identifier_or_literal1left,
identifier_or_literal1right),rest671) end
| (269,(_,(MlyValue.identifier identifier,identifier1left,
identifier1right))::rest671) => let val result=
MlyValue.identifier_or_literal((
Cobol.IDENTIFIER_OR_LITERAL_IS_IDENTIFIER(identifier)))
 in (hojfelds_NT 233,(result,identifier1left,identifier1right),rest671)
 end
| (270,(_,(MlyValue.literal literal,literal1left,literal1right))::
rest671) => let val result=MlyValue.identifier_or_literal((
Cobol.IDENTIFIER_OR_LITERAL_IS_LITERAL(literal)))
 in (hojfelds_NT 233,(result,literal1left,literal1right),rest671) end
| (271,(_,(_,_,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1right)
)::(_,(_,
identifier_or_nonnumeric_literal_or_not_all_figurative_constants1left,
_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 94,(result,
identifier_or_nonnumeric_literal_or_not_all_figurative_constants1left,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1right)
,rest671) end
| (272,(_,(_,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1left,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1right)
)::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 94,(result,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1left,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1right)
,rest671) end
| (273,(_,(_,identifier1left,identifier1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,identifier1left,identifier1right),rest671)
 end
| (274,(_,(_,nonnumeric_literal1left,nonnumeric_literal1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,nonnumeric_literal1left,
nonnumeric_literal1right),rest671) end
| (275,(_,(_,ZERO1left,ZERO1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,ZERO1left,ZERO1right),rest671) end
| (276,(_,(_,ZEROS1left,ZEROS1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,ZEROS1left,ZEROS1right),rest671) end
| (277,(_,(_,ZEROES1left,ZEROES1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,ZEROES1left,ZEROES1right),rest671) end
| (278,(_,(_,SPACE1left,SPACE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,SPACE1left,SPACE1right),rest671) end
| (279,(_,(_,SPACES1left,SPACES1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,SPACES1left,SPACES1right),rest671) end
| (280,(_,(_,HIGHVALUE1left,HIGHVALUE1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,HIGHVALUE1left,HIGHVALUE1right),rest671)
 end
| (281,(_,(_,HIGHVALUES1left,HIGHVALUES1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,HIGHVALUES1left,HIGHVALUES1right),rest671)
 end
| (282,(_,(_,LOWVALUE1left,LOWVALUE1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,LOWVALUE1left,LOWVALUE1right),rest671) end
| (283,(_,(_,LOWVALUES1left,LOWVALUES1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,LOWVALUES1left,LOWVALUES1right),rest671)
 end
| (284,(_,(_,QUOTE1left,QUOTE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,QUOTE1left,QUOTE1right),rest671) end
| (285,(_,(_,QUOTES1left,QUOTES1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 93,(result,QUOTES1left,QUOTES1right),rest671) end
| (286,(_,(_,identifier1left,identifier1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 232,(result,identifier1left,identifier1right),rest671)
 end
| (287,(_,(_,INTEGER1left,INTEGER1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 232,(result,INTEGER1left,INTEGER1right),rest671) end
| (288,(_,(MlyValue.test_cobol_programs test_cobol_programs,
test_cobol_programs1left,test_cobol_programs1right))::rest671) => let 
val result=MlyValue.test_cobol(( Cobol.TEST(test_cobol_programs) ))
 in (hojfelds_NT 3,(result,test_cobol_programs1left,
test_cobol_programs1right),rest671) end
| (289,(_,(MlyValue.cobol_program cobol_program,cobol_program1left,
cobol_program1right))::rest671) => let val result=MlyValue.test_cobol(
( Cobol.COBOL_PROGRAM(cobol_program)))
 in (hojfelds_NT 3,(result,cobol_program1left,cobol_program1right),
rest671) end
| (290,(_,(MlyValue.test_cobol_program test_cobol_program,_,
test_cobol_program1right))::(_,(MlyValue.test_cobol_programs 
test_cobol_programs,test_cobol_programs1left,_))::rest671) => let val 
result=MlyValue.test_cobol_programs((
Cobol.TEST_COBOL_PROGRAMS(test_cobol_programs, test_cobol_program)))
 in (hojfelds_NT 1,(result,test_cobol_programs1left,
test_cobol_program1right),rest671) end
| (291,(_,(MlyValue.test_cobol_program test_cobol_program,
test_cobol_program1left,test_cobol_program1right))::rest671) => let 
val result=MlyValue.test_cobol_programs((
Cobol.TEST_COBOL_PROGRAM(test_cobol_program)))
 in (hojfelds_NT 1,(result,test_cobol_program1left,
test_cobol_program1right),rest671) end
| (292,(_,(_,_,PROGEND1right))::(_,(MlyValue.cobol_program 
cobol_program,_,_))::(_,(_,PROGBEGIN1left,_))::rest671) => let val 
result=MlyValue.test_cobol_program((cobol_program))
 in (hojfelds_NT 2,(result,PROGBEGIN1left,PROGEND1right),rest671) end
| (293,(_,(MlyValue.procedure_division procedure_division,_,
procedure_division1right))::(_,(MlyValue.data_division data_division,_
,_))::(_,(MlyValue.environment_division environment_division,_,_))::(_
,(MlyValue.identification_division identification_division,_,_))::(_,(
_,process_statement1left,_))::rest671) => let val result=
MlyValue.cobol_program((
condition_names := [];
                            Cobol.PROGRAM(identification_division,
                                          environment_division,
                                          data_division,
                                          procedure_division)
))
 in (hojfelds_NT 0,(result,process_statement1left,
procedure_division1right),rest671) end
| (294,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 4,(result,defaultPos,defaultPos),rest671) end
| (295,(_,(_,_,process_options1right))::(_,(_,PROCESS1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 4,(result,PROCESS1left,process_options1right),rest671)
 end
| (296,(_,(_,_,process_option1right))::(_,(_,process_options1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 5,(result,process_options1left,process_option1right),
rest671) end
| (297,(_,(_,process_option1left,process_option1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 5,(result,process_option1left,process_option1right),
rest671) end
| (298,(_,(_,SLASHSYMBOL1left,SLASHSYMBOL1right))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 6,(result,SLASHSYMBOL1left,SLASHSYMBOL1right),rest671)
 end
| (299,(_,(_,SORT1left,SORT1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 6,(result,SORT1left,SORT1right),rest671) end
| (300,(_,(_,SOURCE1left,SOURCE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 6,(result,SOURCE1left,SOURCE1right),rest671) end
| (301,(_,(_,QUOTE1left,QUOTE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 6,(result,QUOTE1left,QUOTE1right),rest671) end
| (302,(_,(_,PERIOD1left,PERIOD1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 6,(result,PERIOD1left,PERIOD1right),rest671) end
| (303,(_,(_,USERDEFINEDWORD1left,USERDEFINEDWORD1right))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 6,(result,USERDEFINEDWORD1left,USERDEFINEDWORD1right),
rest671) end
| (304,(_,(_,COMP1left,COMP1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 6,(result,COMP1left,COMP1right),rest671) end
| (305,(_,(_,LPAR1left,LPAR1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 6,(result,LPAR1left,LPAR1right),rest671) end
| (306,(_,(_,RPAR1left,RPAR1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 6,(result,RPAR1left,RPAR1right),rest671) end
| (307,(_,(_,_,identification_paragraphs1right))::_::_::_::(_,(_,
IDENTIFICATION1left,_))::rest671) => let val result=
MlyValue.identification_division(())
 in (hojfelds_NT 225,(result,IDENTIFICATION1left,
identification_paragraphs1right),rest671) end
| (308,(_,(_,_,PERIOD2right))::_::_::(_,(_,PROGRAMID1left,_))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 104,(result,PROGRAMID1left,PERIOD2right),rest671) end
| (309,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 104,(result,defaultPos,defaultPos),rest671) end
| (310,(_,(_,_,identification_paragraph1right))::(_,(_,
identification_paragraphs1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 227,(result,identification_paragraphs1left,
identification_paragraph1right),rest671) end
| (311,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 227,(result,defaultPos,defaultPos),rest671) end
| (312,(_,(_,author_paragraph1left,author_paragraph1right))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 226,(result,author_paragraph1left,
author_paragraph1right),rest671) end
| (313,(_,(_,installation_paragraph1left,installation_paragraph1right)
)::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 226,(result,installation_paragraph1left,
installation_paragraph1right),rest671) end
| (314,(_,(_,date_written_paragraph1left,date_written_paragraph1right)
)::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 226,(result,date_written_paragraph1left,
date_written_paragraph1right),rest671) end
| (315,(_,(_,date_compiled_paragraph1left,
date_compiled_paragraph1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 226,(result,date_compiled_paragraph1left,
date_compiled_paragraph1right),rest671) end
| (316,(_,(_,security_paragraph1left,security_paragraph1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 226,(result,security_paragraph1left,
security_paragraph1right),rest671) end
| (317,(_,(_,_,comment_entry1right))::(_,(_,AUTHOR1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 50,(result,AUTHOR1left,comment_entry1right),rest671)
 end
| (318,(_,(_,_,comment_entry1right))::(_,(_,INSTALLATION1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 264,(result,INSTALLATION1left,comment_entry1right),
rest671) end
| (319,(_,(_,_,comment_entry1right))::(_,(_,DATEWRITTEN1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 114,(result,DATEWRITTEN1left,comment_entry1right),
rest671) end
| (320,(_,(_,_,comment_entry1right))::(_,(_,DATECOMPILED1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 111,(result,DATECOMPILED1left,comment_entry1right),
rest671) end
| (321,(_,(_,_,comment_entry1right))::(_,(_,SECURITY1left,_))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 426,(result,SECURITY1left,comment_entry1right),rest671
) end
| (322,(_,(MlyValue.input_output_section input_output_section,_,
input_output_section1right))::(_,(MlyValue.configuration_section 
configuration_section,_,_))::_::_::(_,(_,ENVIRONMENT1left,_))::rest671
) => let val result=MlyValue.environment_division((
Cobol.ENVIRONMENT_DIVISION
                   (configuration_section,input_output_section)
))
 in (hojfelds_NT 153,(result,ENVIRONMENT1left,
input_output_section1right),rest671) end
| (323,rest671) => let val result=MlyValue.environment_division((
Cobol.NO_ENVIRONMENT_DIVISION))
 in (hojfelds_NT 153,(result,defaultPos,defaultPos),rest671) end
| (324,(_,(MlyValue.special_names_paragraph special_names_paragraph,_,
special_names_paragraph1right))::(_,(
MlyValue.object_computer_paragraph object_computer_paragraph,_,_))::(_
,(MlyValue.source_computer_paragraph source_computer_paragraph,_,_))::
_::_::(_,(_,CONFIGURATION1left,_))::rest671) => let val result=
MlyValue.configuration_section((
Cobol.CONFIGURATION_SECTION
              (source_computer_paragraph,object_computer_paragraph
               ,special_names_paragraph)
))
 in (hojfelds_NT 77,(result,CONFIGURATION1left,
special_names_paragraph1right),rest671) end
| (325,(_,(_,_,source_computer_entry1right))::_::(_,(_,
SOURCECOMPUTER1left,_))::rest671) => let val result=
MlyValue.source_computer_paragraph(())
 in (hojfelds_NT 451,(result,SOURCECOMPUTER1left,
source_computer_entry1right),rest671) end
| (326,rest671) => let val result=MlyValue.source_computer_paragraph((
))
 in (hojfelds_NT 451,(result,defaultPos,defaultPos),rest671) end
| (327,(_,(_,_,PERIOD1right))::_::(_,(_,computer_name1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 450,(result,computer_name1left,PERIOD1right),rest671)
 end
| (328,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 450,(result,defaultPos,defaultPos),rest671) end
| (329,(_,(_,_,MODE1right))::_::(_,(_,with1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 115,(result,with1left,MODE1right),rest671) end
| (330,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 115,(result,defaultPos,defaultPos),rest671) end
| (331,(_,(_,_,object_computer_entry1right))::_::(_,(_,
OBJECTCOMPUTER1left,_))::rest671) => let val result=
MlyValue.object_computer_paragraph(())
 in (hojfelds_NT 328,(result,OBJECTCOMPUTER1left,
object_computer_entry1right),rest671) end
| (332,rest671) => let val result=MlyValue.object_computer_paragraph((
))
 in (hojfelds_NT 328,(result,defaultPos,defaultPos),rest671) end
| (333,(_,(_,_,PERIOD1right))::_::_::_::(_,(_,computer_name1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 327,(result,computer_name1left,PERIOD1right),rest671)
 end
| (334,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 327,(result,defaultPos,defaultPos),rest671) end
| (335,(_,(_,_,size_unit1right))::_::_::(_,(_,MEMORY1left,_))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 307,(result,MEMORY1left,size_unit1right),rest671) end
| (336,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 307,(result,defaultPos,defaultPos),rest671) end
| (337,(_,(_,WORDS1left,WORDS1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 443,(result,WORDS1left,WORDS1right),rest671) end
| (338,(_,(_,CHARACTERS1left,CHARACTERS1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 443,(result,CHARACTERS1left,CHARACTERS1right),rest671)
 end
| (339,(_,(_,MODULES1left,MODULES1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 443,(result,MODULES1left,MODULES1right),rest671) end
| (340,(_,(_,_,alphabet_name1right))::_::_::(_,(_,
program_collating1left,_))::rest671) => let val result=MlyValue.ntVOID
(())
 in (hojfelds_NT 67,(result,program_collating1left,alphabet_name1right)
,rest671) end
| (341,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 67,(result,defaultPos,defaultPos),rest671) end
| (342,(_,(_,_,INTEGER1right))::_::(_,(_,SEGMENTLIMIT1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 427,(result,SEGMENTLIMIT1left,INTEGER1right),rest671)
 end
| (343,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 427,(result,defaultPos,defaultPos),rest671) end
| (344,(_,(_,_,period1right))::(_,(MlyValue.special_names_clauses 
special_names_clauses,_,_))::_::(_,(_,SPECIALNAMES1left,_))::rest671)
 => let val result=MlyValue.special_names_paragraph((
Cobol.SPECIAL_NAMES_PARAGRAPH(special_names_clauses)))
 in (hojfelds_NT 453,(result,SPECIALNAMES1left,period1right),rest671)
 end
| (345,rest671) => let val result=MlyValue.special_names_paragraph((
Cobol.NO_SPECIAL_NAMES_PARAGRAPH))
 in (hojfelds_NT 453,(result,defaultPos,defaultPos),rest671) end
| (346,(_,(MlyValue.special_names_clause special_names_clause,_,
special_names_clause1right))::(_,(MlyValue.special_names_clauses 
special_names_clauses,special_names_clauses1left,_))::rest671) => let 
val result=MlyValue.special_names_clauses((
Cobol.SEVERAL_SPECIAL_NAMES_CLAUSES(special_names_clauses,
                                               special_names_clause)
))
 in (hojfelds_NT 102,(result,special_names_clauses1left,
special_names_clause1right),rest671) end
| (347,rest671) => let val result=MlyValue.special_names_clauses((
Cobol.NO_SPECIAL_NAMES_CLAUSES))
 in (hojfelds_NT 102,(result,defaultPos,defaultPos),rest671) end
| (348,(_,(_,environment_clause1left,environment_clause1right))::
rest671) => let val result=MlyValue.special_names_clause((
Cobol.ENVIRONMENT_CLAUSE))
 in (hojfelds_NT 103,(result,environment_clause1left,
environment_clause1right),rest671) end
| (349,(_,(_,alphabet_clause1left,alphabet_clause1right))::rest671)
 => let val result=MlyValue.special_names_clause((
Cobol.ALPHABET_CLAUSE))
 in (hojfelds_NT 103,(result,alphabet_clause1left,alphabet_clause1right
),rest671) end
| (350,(_,(MlyValue.currency_sign_clause currency_sign_clause,
currency_sign_clause1left,currency_sign_clause1right))::rest671) => 
let val result=MlyValue.special_names_clause((
Cobol.CURRENCY_SIGN_CLAUSE(currency_sign_clause)))
 in (hojfelds_NT 103,(result,currency_sign_clause1left,
currency_sign_clause1right),rest671) end
| (351,(_,(MlyValue.decimal_clause decimal_clause,decimal_clause1left,
decimal_clause1right))::rest671) => let val result=
MlyValue.special_names_clause((Cobol.DECIMAL_CLAUSE(decimal_clause)))
 in (hojfelds_NT 103,(result,decimal_clause1left,decimal_clause1right),
rest671) end
| (352,(_,(_,_,mnemonic_name1right))::_::(_,(_,environment_name_11left
,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 152,(result,environment_name_11left,
mnemonic_name1right),rest671) end
| (353,(_,(_,_,status_cond_phrase_opt1right))::_::_::(_,(_,
environment_name_21left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 152,(result,environment_name_21left,
status_cond_phrase_opt1right),rest671) end
| (354,(_,(_,_,status_cond_phrase_opt1right))::(_,(_,
environment_name_21left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 152,(result,environment_name_21left,
status_cond_phrase_opt1right),rest671) end
| (355,(_,(_,status_cond_phrase1left,status_cond_phrase1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 463,(result,status_cond_phrase1left,
status_cond_phrase1right),rest671) end
| (356,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 463,(result,defaultPos,defaultPos),rest671) end
| (357,(_,(_,_,off_status_opt1right))::(_,(_,on_status1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 462,(result,on_status1left,off_status_opt1right),
rest671) end
| (358,(_,(_,_,on_status_opt1right))::(_,(_,off_status1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 462,(result,off_status1left,on_status_opt1right),
rest671) end
| (359,(_,(_,on_status1left,on_status1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 340,(result,on_status1left,on_status1right),rest671)
 end
| (360,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 340,(result,defaultPos,defaultPos),rest671) end
| (361,(_,(_,off_status1left,off_status1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 333,(result,off_status1left,off_status1right),rest671)
 end
| (362,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 333,(result,defaultPos,defaultPos),rest671) end
| (363,(_,(_,_,condition_name1right))::_::(_,(_,ON1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 339,(result,ON1left,condition_name1right),rest671) end
| (364,(_,(_,_,condition_name1right))::_::(_,(_,OFF1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 332,(result,OFF1left,condition_name1right),rest671)
 end
| (365,(_,(_,_,alphabet_specifier1right))::_::(_,(_,alphabet_name1left
,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 23,(result,alphabet_name1left,alphabet_specifier1right
),rest671) end
| (366,(_,(_,STANDARD11left,STANDARD11right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 25,(result,STANDARD11left,STANDARD11right),rest671)
 end
| (367,(_,(_,NATIVE1left,NATIVE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 25,(result,NATIVE1left,NATIVE1right),rest671) end
| (368,(_,(_,literal_also_ranges1left,literal_also_ranges1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 25,(result,literal_also_ranges1left,
literal_also_ranges1right),rest671) end
| (369,(_,(_,_,literal_also_range1right))::(_,(_,
literal_also_ranges1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 300,(result,literal_also_ranges1left,
literal_also_range1right),rest671) end
| (370,(_,(_,literal_also_range1left,literal_also_range1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 300,(result,literal_also_range1left,
literal_also_range1right),rest671) end
| (371,(_,(_,_,literal_boundary1right))::(_,(_,literal1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 299,(result,literal1left,literal_boundary1right),
rest671) end
| (372,(_,(_,_,literal1right))::(_,(_,through1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 301,(result,through1left,literal1right),rest671) end
| (373,(_,(_,also_literals1left,also_literals1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 301,(result,also_literals1left,also_literals1right),
rest671) end
| (374,(_,(_,_,also_literal1right))::(_,(_,also_literals1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 27,(result,also_literals1left,also_literal1right),
rest671) end
| (375,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 27,(result,defaultPos,defaultPos),rest671) end
| (376,(_,(_,_,literal1right))::(_,(_,ALSO1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 26,(result,ALSO1left,literal1right),rest671) end
| (377,(_,(MlyValue.literal_range literal_range,_,literal_range1right)
)::(_,(MlyValue.literal_ranges literal_ranges,literal_ranges1left,_))
::rest671) => let val result=MlyValue.literal_ranges((
Cobol.SEVERAL_LITERAL_RANGES(literal_ranges,literal_range)))
 in (hojfelds_NT 303,(result,literal_ranges1left,literal_range1right),
rest671) end
| (378,(_,(MlyValue.literal_range literal_range,literal_range1left,
literal_range1right))::rest671) => let val result=
MlyValue.literal_ranges((Cobol.ONE_LITERAL_RANGE(literal_range)))
 in (hojfelds_NT 303,(result,literal_range1left,literal_range1right),
rest671) end
| (379,(_,(MlyValue.through_literal through_literal,_,
through_literal1right))::(_,(MlyValue.literal literal,literal1left,_))
::rest671) => let val result=MlyValue.literal_range((
Cobol.LITERAL_RANGE(literal,through_literal)))
 in (hojfelds_NT 302,(result,literal1left,through_literal1right),
rest671) end
| (380,(_,(MlyValue.literal literal,_,literal1right))::(_,(_,
through1left,_))::rest671) => let val result=MlyValue.through_literal(
(Cobol.THROUGH_LITERAL(literal)))
 in (hojfelds_NT 493,(result,through1left,literal1right),rest671) end
| (381,rest671) => let val result=MlyValue.through_literal((
Cobol.NO_THROUGH_LITERAL))
 in (hojfelds_NT 493,(result,defaultPos,defaultPos),rest671) end
| (382,(_,(MlyValue.NONNUMERICLITERAL NONNUMERICLITERAL,_,
NONNUMERICLITERAL1right))::_::(_,(_,CURRENCY1left,_))::rest671) => 
let val result=MlyValue.currency_sign_clause((
Cobol.CURRENCY(NONNUMERICLITERAL)))
 in (hojfelds_NT 85,(result,CURRENCY1left,NONNUMERICLITERAL1right),
rest671) end
| (383,(_,(_,_,COMMA1right))::_::(_,(_,DECIMALPOINT1left,_))::rest671)
 => let val result=MlyValue.decimal_clause((
Cobol.DECIMALPOINT_IS_COMMA))
 in (hojfelds_NT 116,(result,DECIMALPOINT1left,COMMA1right),rest671)
 end
| (384,(_,(_,_,i_o_control_paragraph1right))::_::_::_::(_,(_,
INPUTOUTPUT1left,_))::rest671) => let val result=
MlyValue.input_output_section(())
 in (hojfelds_NT 260,(result,INPUTOUTPUT1left,
i_o_control_paragraph1right),rest671) end
| (385,rest671) => let val result=MlyValue.input_output_section(())
 in (hojfelds_NT 260,(result,defaultPos,defaultPos),rest671) end
| (386,(_,(_,_,file_control_entries1right))::_::(_,(_,FILECONTROL1left
,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 189,(result,FILECONTROL1left,
file_control_entries1right),rest671) end
| (387,(_,(_,_,file_control_entry1right))::(_,(_,
file_control_entries1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 187,(result,file_control_entries1left,
file_control_entry1right),rest671) end
| (388,(_,(_,file_control_entry1left,file_control_entry1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 187,(result,file_control_entry1left,
file_control_entry1right),rest671) end
| (389,(_,(_,_,PERIOD1right))::_::_::_::(_,(_,SELECT1left,_))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 188,(result,SELECT1left,PERIOD1right),rest671) end
| (390,(_,(_,_,file_control_clause1right))::(_,(_,
file_control_clauses1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 186,(result,file_control_clauses1left,
file_control_clause1right),rest671) end
| (391,(_,(_,file_control_clause1left,file_control_clause1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 186,(result,file_control_clause1left,
file_control_clause1right),rest671) end
| (392,(_,(_,assign_clause1left,assign_clause1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 185,(result,assign_clause1left,assign_clause1right),
rest671) end
| (393,(_,(_,reserve_clause1left,reserve_clause1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 185,(result,reserve_clause1left,reserve_clause1right),
rest671) end
| (394,(_,(_,organization_clause1left,organization_clause1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 185,(result,organization_clause1left,
organization_clause1right),rest671) end
| (395,(_,(_,record_key_clause1left,record_key_clause1right))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 185,(result,record_key_clause1left,
record_key_clause1right),rest671) end
| (396,(_,(_,access_mode_clause1left,access_mode_clause1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 185,(result,access_mode_clause1left,
access_mode_clause1right),rest671) end
| (397,(_,(_,file_status_clause1left,file_status_clause1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 185,(result,file_status_clause1left,
file_status_clause1right),rest671) end
| (398,(_,(_,control_clause1left,control_clause1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 185,(result,control_clause1left,control_clause1right),
rest671) end
| (399,(_,(_,_,external_data_sets1right))::_::(_,(_,ASSIGN1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 46,(result,ASSIGN1left,external_data_sets1right),
rest671) end
| (400,(_,(_,_,external_data_set1right))::(_,(_,
external_data_sets1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 165,(result,external_data_sets1left,
external_data_set1right),rest671) end
| (401,(_,(_,external_data_set1left,external_data_set1right))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 165,(result,external_data_set1left,
external_data_set1right),rest671) end
| (402,(_,(_,assignment_name1left,assignment_name1right))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 164,(result,assignment_name1left,assignment_name1right
),rest671) end
| (403,(_,(_,NONNUMERICLITERAL1left,NONNUMERICLITERAL1right))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 164,(result,NONNUMERICLITERAL1left,
NONNUMERICLITERAL1right),rest671) end
| (404,(_,(_,_,areas1right))::_::(_,(_,RESERVE1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 410,(result,RESERVE1left,areas1right),rest671) end
| (405,(_,(_,_,data_organization1right))::(_,(_,organization_is1left,_
))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 351,(result,organization_is1left,
data_organization1right),rest671) end
| (406,(_,(_,SEQUENTIAL1left,SEQUENTIAL1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 107,(result,SEQUENTIAL1left,SEQUENTIAL1right),rest671)
 end
| (407,(_,(_,INDEXED1left,INDEXED1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 107,(result,INDEXED1left,INDEXED1right),rest671) end
| (408,(_,(_,RELATIVE1left,RELATIVE1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 107,(result,RELATIVE1left,RELATIVE1right),rest671) end
| (409,(_,(_,TRANSACTION1left,TRANSACTION1right))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 107,(result,TRANSACTION1left,TRANSACTION1right),
rest671) end
| (410,(_,(_,_,relative_key_clause1right))::_::_::(_,(_,ACCESS1left,_)
)::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 8,(result,ACCESS1left,relative_key_clause1right),
rest671) end
| (411,(_,(_,SEQUENTIAL1left,SEQUENTIAL1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 9,(result,SEQUENTIAL1left,SEQUENTIAL1right),rest671)
 end
| (412,(_,(_,RANDOM1left,RANDOM1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 9,(result,RANDOM1left,RANDOM1right),rest671) end
| (413,(_,(_,DYNAMIC1left,DYNAMIC1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 9,(result,DYNAMIC1left,DYNAMIC1right),rest671) end
| (414,(_,(_,_,data_name1right))::_::_::(_,(_,RELATIVE1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 396,(result,RELATIVE1left,data_name1right),rest671)
 end
| (415,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 396,(result,defaultPos,defaultPos),rest671) end
| (416,(_,(_,_,duplicates_phrase1right))::_::_::_::(_,(_,RECORD1left,_
))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 388,(result,RECORD1left,duplicates_phrase1right),
rest671) end
| (417,(_,(_,_,DUPLICATES1right))::(_,(_,with1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 128,(result,with1left,DUPLICATES1right),rest671) end
| (418,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 128,(result,defaultPos,defaultPos),rest671) end
| (419,(_,(_,_,data_name_opt1right))::_::_::_::(_,(_,file1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 210,(result,file1left,data_name_opt1right),rest671)
 end
| (420,(_,(_,_,data_name1right))::_::(_,(_,CONTROLAREA1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 79,(result,CONTROLAREA1left,data_name1right),rest671)
 end
| (421,(_,(_,_,i_o_control_entries1right))::_::(_,(_,IOCONTROL1left,_)
)::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 223,(result,IOCONTROL1left,i_o_control_entries1right),
rest671) end
| (422,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 223,(result,defaultPos,defaultPos),rest671) end
| (423,(_,(_,_,PERIOD1right))::(_,(_,i_o_control_clauses1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 222,(result,i_o_control_clauses1left,PERIOD1right),
rest671) end
| (424,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 222,(result,defaultPos,defaultPos),rest671) end
| (425,(_,(_,_,i_o_control_clause1right))::(_,(_,
i_o_control_clauses1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 221,(result,i_o_control_clauses1left,
i_o_control_clause1right),rest671) end
| (426,(_,(_,i_o_control_clause1left,i_o_control_clause1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 221,(result,i_o_control_clause1left,
i_o_control_clause1right),rest671) end
| (427,(_,(_,rerun_clause1left,rerun_clause1right))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 220,(result,rerun_clause1left,rerun_clause1right),
rest671) end
| (428,(_,(_,same_area_clause1left,same_area_clause1right))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 220,(result,same_area_clause1left,
same_area_clause1right),rest671) end
| (429,(_,(_,same_record_area_clause1left,
same_record_area_clause1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 220,(result,same_record_area_clause1left,
same_record_area_clause1right),rest671) end
| (430,(_,(_,same_sort_area_clause1left,same_sort_area_clause1right))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 220,(result,same_sort_area_clause1left,
same_sort_area_clause1right),rest671) end
| (431,(_,(_,same_sort_merge_area_clause1left,
same_sort_merge_area_clause1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 220,(result,same_sort_merge_area_clause1left,
same_sort_merge_area_clause1right),rest671) end
| (432,(_,(_,apply_core_index_clause1left,
apply_core_index_clause1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 220,(result,apply_core_index_clause1left,
apply_core_index_clause1right),rest671) end
| (433,(_,(_,multiple_file_tape_clause1left,
multiple_file_tape_clause1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 220,(result,multiple_file_tape_clause1left,
multiple_file_tape_clause1right),rest671) end
| (434,(_,(_,_,file_name2right))::_::_::_::_::_::_::(_,(_,RERUN1left,_
))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 409,(result,RERUN1left,file_name2right),rest671) end
| (435,(_,(_,_,file_names1right))::_::_::(_,(_,SAME1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 416,(result,SAME1left,file_names1right),rest671) end
| (436,(_,(_,_,file_names1right))::_::_::_::(_,(_,SAME1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 417,(result,SAME1left,file_names1right),rest671) end
| (437,(_,(_,_,file_names1right))::_::_::_::(_,(_,SAME1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 419,(result,SAME1left,file_names1right),rest671) end
| (438,(_,(_,_,file_names1right))::_::_::_::(_,(_,SAME1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 420,(result,SAME1left,file_names1right),rest671) end
| (439,(_,(_,_,file_names1right))::_::_::_::_::(_,(_,APPLY1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 37,(result,APPLY1left,file_names1right),rest671) end
| (440,(_,(_,_,file_positions1right))::_::_::(_,(_,MULTIPLE1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 316,(result,MULTIPLE1left,file_positions1right),
rest671) end
| (441,(_,(_,_,file_position1right))::(_,(_,file_positions1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 208,(result,file_positions1left,file_position1right),
rest671) end
| (442,(_,(_,file_position1left,file_position1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 208,(result,file_position1left,file_position1right),
rest671) end
| (443,(_,(_,_,position_integer1right))::(_,(_,file_name1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 207,(result,file_name1left,position_integer1right),
rest671) end
| (444,(_,(_,_,INTEGER1right))::(_,(_,POSITION1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 365,(result,POSITION1left,INTEGER1right),rest671) end
| (445,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 365,(result,defaultPos,defaultPos),rest671) end
| (446,(_,(MlyValue.linkage_section linkage_section,_,
linkage_section1right))::(_,(MlyValue.working_storage_section 
working_storage_section,_,_))::(_,(MlyValue.file_section file_section,
_,_))::_::_::(_,(_,DATA1left,_))::rest671) => let val result=
MlyValue.data_division((
Cobol.DATA_DIVISION(file_section,
                                             working_storage_section,
                                             linkage_section)
))
 in (hojfelds_NT 91,(result,DATA1left,linkage_section1right),rest671)
 end
| (447,rest671) => let val result=MlyValue.data_division((
Cobol.NO_DATA_DIVISION))
 in (hojfelds_NT 91,(result,defaultPos,defaultPos),rest671) end
| (448,(_,(MlyValue.file_description_paragraphs 
file_description_paragraphs,_,file_description_paragraphsright as 
file_description_paragraphs1right))::_::_::(_,(_,FILEleft as FILE1left
,_))::rest671) => let val result=MlyValue.file_section((
Cobol.FILE_SECTION(file_description_paragraphs,FILEleft,file_description_paragraphsright)
))
 in (hojfelds_NT 209,(result,FILE1left,
file_description_paragraphs1right),rest671) end
| (449,rest671) => let val result=MlyValue.file_section((
Cobol.NO_FILE_SECTION))
 in (hojfelds_NT 209,(result,defaultPos,defaultPos),rest671) end
| (450,(_,(MlyValue.file_description_paragraph 
file_description_paragraph,_,file_description_paragraph1right))::(_,(
MlyValue.file_description_paragraphs file_description_paragraphs,
file_description_paragraphs1left,_))::rest671) => let val result=
MlyValue.file_description_paragraphs((
Cobol.FILE_DESCRIPTION_PARAGRAPHS(file_description_paragraphs,
                                              file_description_paragraph)
))
 in (hojfelds_NT 195,(result,file_description_paragraphs1left,
file_description_paragraph1right),rest671) end
| (451,rest671) => let val result=MlyValue.file_description_paragraphs
((Cobol.NO_FILE_DESCRIPTION_PARAGRAPHS))
 in (hojfelds_NT 195,(result,defaultPos,defaultPos),rest671) end
| (452,(_,(MlyValue.record_description_entries 
record_description_entries,_,record_description_entries1right))::(_,(
MlyValue.file_and_sort_description_entry 
file_and_sort_description_entry,file_and_sort_description_entry1left,_
))::rest671) => let val result=MlyValue.file_description_paragraph((
Cobol.FILE_DESCRIPTION_PARAGRAPH(file_and_sort_description_entry,
                                            record_description_entries)
))
 in (hojfelds_NT 194,(result,file_and_sort_description_entry1left,
record_description_entries1right),rest671) end
| (453,(_,(MlyValue.file_description_entry file_description_entry,
file_description_entry1left,file_description_entry1right))::rest671)
 => let val result=MlyValue.file_and_sort_description_entry((
Cobol.FILE_DESCRIPTION_ENTRY(file_description_entry)))
 in (hojfelds_NT 184,(result,file_description_entry1left,
file_description_entry1right),rest671) end
| (454,(_,(MlyValue.sort_description_entry sort_description_entry,
sort_description_entry1left,sort_description_entry1right))::rest671)
 => let val result=MlyValue.file_and_sort_description_entry((
Cobol.SORT_DESCRIPTION_ENTRY(sort_description_entry)))
 in (hojfelds_NT 184,(result,sort_description_entry1left,
sort_description_entry1right),rest671) end
| (455,(_,(_,_,PERIODright as PERIOD1right))::(_,(
MlyValue.file_description_clauses file_description_clauses,_,_))::(_,(
MlyValue.file_name file_name,_,_))::(_,(_,FDleft as FD1left,_))::
rest671) => let val result=MlyValue.file_description_entry((
Cobol.FD(file_name,file_description_clauses,FDleft,PERIODright)))
 in (hojfelds_NT 193,(result,FD1left,PERIOD1right),rest671) end
| (456,(_,(MlyValue.file_description_clause file_description_clause,_,
file_description_clause1right))::(_,(MlyValue.file_description_clauses
 file_description_clauses,file_description_clauses1left,_))::rest671)
 => let val result=MlyValue.file_description_clauses((
Cobol.FILE_DESCRIPTION_CLAUSES(file_description_clauses,
                                        file_description_clause)
))
 in (hojfelds_NT 192,(result,file_description_clauses1left,
file_description_clause1right),rest671) end
| (457,rest671) => let val result=MlyValue.file_description_clauses((
Cobol.NO_FILE_DESCRIPTION_CLAUSES))
 in (hojfelds_NT 192,(result,defaultPos,defaultPos),rest671) end
| (458,(_,(MlyValue.block_contains_clause block_contains_clause,
block_contains_clause1left,block_contains_clause1right))::rest671) => 
let val result=MlyValue.file_description_clause((block_contains_clause
))
 in (hojfelds_NT 191,(result,block_contains_clause1left,
block_contains_clause1right),rest671) end
| (459,(_,(MlyValue.record_contains_clause record_contains_clause,
record_contains_clauseleft as record_contains_clause1left,
record_contains_clauseright as record_contains_clause1right))::rest671
) => let val result=MlyValue.file_description_clause((
Cobol.FILE_DESCRIPTION_CLAUSE_IS_RECORD(record_contains_clause,
                                                   record_contains_clauseleft,
                                                   record_contains_clauseright)
))
 in (hojfelds_NT 191,(result,record_contains_clause1left,
record_contains_clause1right),rest671) end
| (460,(_,(MlyValue.label_records_clause label_records_clause,
label_records_clause1left,label_records_clause1right))::rest671) => 
let val result=MlyValue.file_description_clause((label_records_clause)
)
 in (hojfelds_NT 191,(result,label_records_clause1left,
label_records_clause1right),rest671) end
| (461,(_,(MlyValue.value_of_clause value_of_clause,
value_of_clause1left,value_of_clause1right))::rest671) => let val 
result=MlyValue.file_description_clause((value_of_clause))
 in (hojfelds_NT 191,(result,value_of_clause1left,value_of_clause1right
),rest671) end
| (462,(_,(MlyValue.data_records_clause data_records_clause,
data_records_clauseleft as data_records_clause1left,
data_records_clauseright as data_records_clause1right))::rest671) => 
let val result=MlyValue.file_description_clause((
Cobol.FILE_DESCRIPTION_CLAUSE_IS_DATA(data_records_clause,
                                                 data_records_clauseleft,
                                                 data_records_clauseright)
))
 in (hojfelds_NT 191,(result,data_records_clause1left,
data_records_clause1right),rest671) end
| (463,(_,(MlyValue.linage_clause linage_clause,linage_clause1left,
linage_clause1right))::rest671) => let val result=
MlyValue.file_description_clause((linage_clause))
 in (hojfelds_NT 191,(result,linage_clause1left,linage_clause1right),
rest671) end
| (464,(_,(MlyValue.code_set_clause code_set_clause,
code_set_clause1left,code_set_clause1right))::rest671) => let val 
result=MlyValue.file_description_clause((code_set_clause))
 in (hojfelds_NT 191,(result,code_set_clause1left,code_set_clause1right
),rest671) end
| (465,(_,(MlyValue.characters_or_records characters_or_records,_,
characters_or_recordsright as characters_or_records1right))::(_,(
MlyValue.integer_range integer_range,_,_))::_::(_,(_,BLOCKleft as 
BLOCK1left,_))::rest671) => let val result=
MlyValue.block_contains_clause((
Cobol.BLOCK(integer_range,
                     characters_or_records,
                     BLOCKleft,
                     characters_or_recordsright)
))
 in (hojfelds_NT 53,(result,BLOCK1left,characters_or_records1right),
rest671) end
| (466,(_,(MlyValue.INTEGER INTEGER,INTEGER1left,INTEGER1right))::
rest671) => let val result=MlyValue.integer_range((
Cobol.SIMPLE_RANGE(INTEGER)))
 in (hojfelds_NT 265,(result,INTEGER1left,INTEGER1right),rest671) end
| (467,(_,(MlyValue.INTEGER INTEGER2,_,INTEGER2right))::_::(_,(
MlyValue.INTEGER INTEGER1,INTEGER1left,_))::rest671) => let val result
=MlyValue.integer_range((Cobol.INTEGER_TO_INTEGER(INTEGER1,INTEGER2)))
 in (hojfelds_NT 265,(result,INTEGER1left,INTEGER2right),rest671) end
| (468,(_,(_,characters1left,characters1right))::rest671) => let val 
result=MlyValue.characters_or_records((Cobol.CHARACTERS))
 in (hojfelds_NT 62,(result,characters1left,characters1right),rest671)
 end
| (469,(_,(_,RECORDS1left,RECORDS1right))::rest671) => let val result=
MlyValue.characters_or_records((Cobol.RECORDS))
 in (hojfelds_NT 62,(result,RECORDS1left,RECORDS1right),rest671) end
| (470,(_,(_,_,characters1right))::(_,(MlyValue.integer_range 
integer_range,_,_))::_::(_,(_,RECORD1left,_))::rest671) => let val 
result=MlyValue.record_contains_clause((
Cobol.RECORD_CONTAINS(integer_range)))
 in (hojfelds_NT 381,(result,RECORD1left,characters1right),rest671) end
| (471,(_,(MlyValue.standard_or_omitted standard_or_omitted,_,
standard_or_omitted1right))::_::(_,(_,LABEL1left,_))::rest671) => let 
val result=MlyValue.label_records_clause((standard_or_omitted))
 in (hojfelds_NT 285,(result,LABEL1left,standard_or_omitted1right),
rest671) end
| (472,(_,(_,_,is1right))::(_,(_,RECORD1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 387,(result,RECORD1left,is1right),rest671) end
| (473,(_,(_,_,are1right))::(_,(_,RECORDS1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 387,(result,RECORDS1left,are1right),rest671) end
| (474,(_,(_,STANDARD1left,STANDARD1right))::rest671) => let val 
result=MlyValue.standard_or_omitted((Cobol.LABEL_STANDARD))
 in (hojfelds_NT 456,(result,STANDARD1left,STANDARD1right),rest671) end
| (475,(_,(_,OMITTED1left,OMITTED1right))::rest671) => let val result=
MlyValue.standard_or_omitted((Cobol.LABEL_OMITTED))
 in (hojfelds_NT 456,(result,OMITTED1left,OMITTED1right),rest671) end
| (476,(_,(MlyValue.value_of_phrases value_of_phrases,_,
value_of_phrasesright as value_of_phrases1right))::_::(_,(_,VALUEleft
 as VALUE1left,_))::rest671) => let val result=
MlyValue.value_of_clause((
Cobol.VALUE_OF_CLAUSE(value_of_phrases,
                                 VALUEleft,
                                 value_of_phrasesright)
))
 in (hojfelds_NT 518,(result,VALUE1left,value_of_phrases1right),rest671
) end
| (477,(_,(MlyValue.value_of_phrase value_of_phrase,_,
value_of_phrase1right))::(_,(MlyValue.value_of_phrases 
value_of_phrases,value_of_phrases1left,_))::rest671) => let val result
=MlyValue.value_of_phrases((
Cobol.VALUE_OF_PHRASES(value_of_phrases, value_of_phrase)))
 in (hojfelds_NT 521,(result,value_of_phrases1left,
value_of_phrase1right),rest671) end
| (478,(_,(MlyValue.value_of_phrase value_of_phrase,
value_of_phrase1left,value_of_phrase1right))::rest671) => let val 
result=MlyValue.value_of_phrases((
Cobol.VALUE_OF_PHRASE(value_of_phrase)))
 in (hojfelds_NT 521,(result,value_of_phrase1left,value_of_phrase1right
),rest671) end
| (479,(_,(MlyValue.data_name_or_literal data_name_or_literal,_,
data_name_or_literal1right))::_::(_,(MlyValue.system_name system_name,
system_name1left,_))::rest671) => let val result=
MlyValue.value_of_phrase((
Cobol.VALUE_OF(system_name, data_name_or_literal)))
 in (hojfelds_NT 520,(result,system_name1left,
data_name_or_literal1right),rest671) end
| (480,(_,(MlyValue.data_names data_names,_,data_names1right))::_::(_,
(_,DATA1left,_))::rest671) => let val result=
MlyValue.data_records_clause((Cobol.DATA_RECORD_IS(data_names)))
 in (hojfelds_NT 108,(result,DATA1left,data_names1right),rest671) end
| (481,(_,(MlyValue.top_bottom_specifications 
top_bottom_specifications,_,top_bottom_specificationsright as 
top_bottom_specifications1right))::(_,(MlyValue.footing_specification 
footing_specification,_,_))::_::(_,(MlyValue.data_name_or_integer 
data_name_or_integer,_,_))::_::(_,(_,LINAGEleft as LINAGE1left,_))::
rest671) => let val result=MlyValue.linage_clause((
Cobol.LINAGE_CLAUSE(data_name_or_integer,
                                 footing_specification,
                                 top_bottom_specifications,
                                 LINAGEleft,
                                 top_bottom_specificationsright)
))
 in (hojfelds_NT 293,(result,LINAGE1left,
top_bottom_specifications1right),rest671) end
| (482,(_,(MlyValue.data_name_or_integer data_name_or_integer,_,
data_name_or_integer1right))::_::_::(_,(_,with1left,_))::rest671) => 
let val result=MlyValue.footing_specification((
Cobol.FOOTING(data_name_or_integer)))
 in (hojfelds_NT 212,(result,with1left,data_name_or_integer1right),
rest671) end
| (483,rest671) => let val result=MlyValue.footing_specification((
Cobol.NOFOOTING))
 in (hojfelds_NT 212,(result,defaultPos,defaultPos),rest671) end
| (484,(_,(MlyValue.top_bottom_specification top_bottom_specification,
_,top_bottom_specification1right))::(_,(
MlyValue.top_bottom_specifications top_bottom_specifications,
top_bottom_specifications1left,_))::rest671) => let val result=
MlyValue.top_bottom_specifications((
Cobol.TOP_BOTTOM_SPECIFICATIONS(top_bottom_specifications,
                                            top_bottom_specification)
))
 in (hojfelds_NT 502,(result,top_bottom_specifications1left,
top_bottom_specification1right),rest671) end
| (485,rest671) => let val result=MlyValue.top_bottom_specifications((
Cobol.NO_TOP_BOTTOM_SPECIFICATION))
 in (hojfelds_NT 502,(result,defaultPos,defaultPos),rest671) end
| (486,(_,(_,_,data_name_or_integer1right))::(_,(MlyValue.top_bottom 
top_bottom,_,_))::_::(_,(_,lines1left,_))::rest671) => let val result=
MlyValue.top_bottom_specification((top_bottom))
 in (hojfelds_NT 501,(result,lines1left,data_name_or_integer1right),
rest671) end
| (487,(_,(_,TOP1left,TOP1right))::rest671) => let val result=
MlyValue.top_bottom((Cobol.TOP))
 in (hojfelds_NT 500,(result,TOP1left,TOP1right),rest671) end
| (488,(_,(_,BOTTOM1left,BOTTOM1right))::rest671) => let val result=
MlyValue.top_bottom((Cobol.BOTTOM))
 in (hojfelds_NT 500,(result,BOTTOM1left,BOTTOM1right),rest671) end
| (489,(_,(MlyValue.alphabet_name alphabet_name,_,alphabet_nameright
 as alphabet_name1right))::_::(_,(_,CODESETleft as CODESET1left,_))::
rest671) => let val result=MlyValue.code_set_clause((
Cobol.CODE_SET_CLAUSE(alphabet_name,
                                 CODESETleft,
                                 alphabet_nameright)
))
 in (hojfelds_NT 65,(result,CODESET1left,alphabet_name1right),rest671)
 end
| (490,(_,(_,_,PERIOD1right))::(_,(MlyValue.sort_description_clauses 
sort_description_clauses,_,_))::(_,(MlyValue.file_name file_name,_,_))
::(_,(_,SD1left,_))::rest671) => let val result=
MlyValue.sort_description_entry((
Cobol.SD(file_name,sort_description_clauses)))
 in (hojfelds_NT 447,(result,SD1left,PERIOD1right),rest671) end
| (491,(_,(MlyValue.sort_description_clause sort_description_clause,_,
sort_description_clause1right))::(_,(MlyValue.sort_description_clauses
 sort_description_clauses,sort_description_clauses1left,_))::rest671)
 => let val result=MlyValue.sort_description_clauses((
Cobol.SORT_DESCRIPTION_CLAUSES(sort_description_clauses,
                                          sort_description_clause)
))
 in (hojfelds_NT 446,(result,sort_description_clauses1left,
sort_description_clause1right),rest671) end
| (492,rest671) => let val result=MlyValue.sort_description_clauses((
Cobol.NO_SORT_DESCRIPTION_CLAUSE))
 in (hojfelds_NT 446,(result,defaultPos,defaultPos),rest671) end
| (493,(_,(MlyValue.record_contains_clause record_contains_clause,
record_contains_clauseleft as record_contains_clause1left,
record_contains_clauseright as record_contains_clause1right))::rest671
) => let val result=MlyValue.sort_description_clause((
Cobol.SORT_DESCRIPTION_CLAUSE_IS_RECORD
                (record_contains_clause,
                 record_contains_clauseleft,
                 record_contains_clauseright)
))
 in (hojfelds_NT 445,(result,record_contains_clause1left,
record_contains_clause1right),rest671) end
| (494,(_,(MlyValue.data_records_clause data_records_clause,
data_records_clauseleft as data_records_clause1left,
data_records_clauseright as data_records_clause1right))::rest671) => 
let val result=MlyValue.sort_description_clause((
Cobol.SORT_DESCRIPTION_CLAUSE_IS_DATA(data_records_clause,
                                                 data_records_clauseleft,
                                                 data_records_clauseright)
))
 in (hojfelds_NT 445,(result,data_records_clause1left,
data_records_clause1right),rest671) end
| (495,(_,(MlyValue.record_description_entry record_description_entry,
_,record_description_entry1right))::(_,(
MlyValue.record_description_entries record_description_entries,
record_description_entries1left,_))::rest671) => let val result=
MlyValue.record_description_entries((
Cobol.RECORD_DESCRIPTION_ENTRIES(record_description_entries,
                                            record_description_entry)
))
 in (hojfelds_NT 385,(result,record_description_entries1left,
record_description_entry1right),rest671) end
| (496,(_,(MlyValue.record_description_entry record_description_entry,
record_description_entry1left,record_description_entry1right))::
rest671) => let val result=MlyValue.record_description_entries((
Cobol.RECORD_DESCRIPTION_ENTRIES(Cobol.NO_RECORD_DESCRIPTION_ENTRIES,
                                           record_description_entry)
))
 in (hojfelds_NT 385,(result,record_description_entry1left,
record_description_entry1right),rest671) end
| (497,(_,(MlyValue.record_description_entries 
record_description_entries,record_description_entries1left,
record_description_entries1right))::rest671) => let val result=
MlyValue.record_description_entries_opt((
Cobol.RECORD_DESCRIPTION_ENTRIES_OPT(record_description_entries)))
 in (hojfelds_NT 384,(result,record_description_entries1left,
record_description_entries1right),rest671) end
| (498,rest671) => let val result=
MlyValue.record_description_entries_opt((
Cobol.NO_RECORD_DESCRIPTION_ENTRIES_OPT))
 in (hojfelds_NT 384,(result,defaultPos,defaultPos),rest671) end
| (499,(_,(MlyValue.data_description_entry data_description_entry,
data_description_entry1left,data_description_entry1right))::rest671)
 => let val result=MlyValue.record_description_entry((
data_description_entry))
 in (hojfelds_NT 386,(result,data_description_entry1left,
data_description_entry1right),rest671) end
| (500,(_,(MlyValue.data_description_entry_134 
data_description_entry_134,data_description_entry_1341left,
data_description_entry_1341right))::rest671) => let val result=
MlyValue.data_description_entry((data_description_entry_134))
 in (hojfelds_NT 89,(result,data_description_entry_1341left,
data_description_entry_1341right),rest671) end
| (501,(_,(MlyValue.renames_clause renames_clause,renames_clause1left,
renames_clause1right))::rest671) => let val result=
MlyValue.data_description_entry((renames_clause))
 in (hojfelds_NT 89,(result,renames_clause1left,renames_clause1right),
rest671) end
| (502,(_,(MlyValue.data_description_annotation 
data_description_annotation,data_description_annotation1left,
data_description_annotation1right))::rest671) => let val result=
MlyValue.data_description_entry((data_description_annotation))
 in (hojfelds_NT 89,(result,data_description_annotation1left,
data_description_annotation1right),rest671) end
| (503,(_,(MlyValue.ts2k ts2k,_,ts2kright as ts2k1right))::(_,(_,
TS2Kleft as TS2K1left,_))::rest671) => let val result=
MlyValue.data_description_annotation((
Cobol.TS2K_NOARROW(ts2k,TS2Kleft,ts2kright)))
 in (hojfelds_NT 541,(result,TS2K1left,ts2k1right),rest671) end
| (504,(_,(MlyValue.ts2k ts2k2,_,ts2k2right))::_::(_,(MlyValue.ts2k 
ts2k1,_,_))::(_,(_,TS2Kleft as TS2K1left,_))::rest671) => let val 
result=MlyValue.data_description_annotation((
Cobol.TS2K_ARROW(ts2k1,ts2k2,TS2Kleft,ts2k2right)))
 in (hojfelds_NT 541,(result,TS2K1left,ts2k2right),rest671) end
| (505,(_,(MlyValue.ts2k ts2k2,_,ts2k2right))::_::(_,(MlyValue.ts2k 
ts2k1,_,_))::_::(_,(_,TS2Kleft as TS2K1left,_))::rest671) => let val 
result=MlyValue.data_description_annotation((
Cobol.TS2K_ALL(ts2k1,ts2k2,TS2Kleft,ts2k2right)))
 in (hojfelds_NT 541,(result,TS2K1left,ts2k2right),rest671) end
| (506,(_,(_,_,ENDright as END1right))::(_,(_,TS2Kleft as TS2K1left,_)
)::rest671) => let val result=MlyValue.data_description_annotation((
Cobol.TS2K_END(TS2Kleft,ENDright)))
 in (hojfelds_NT 541,(result,TS2K1left,END1right),rest671) end
| (507,(_,(_,_,SEPARATEright as SEPARATE1right))::_::(_,(_,TS2Kleft
 as TS2K1left,_))::rest671) => let val result=
MlyValue.data_description_annotation((
Cobol.TS2K_ASSUME_SEPARATE(TS2Kleft,SEPARATEright)))
 in (hojfelds_NT 541,(result,TS2K1left,SEPARATE1right),rest671) end
| (508,(_,(_,_,PERIODright as PERIOD1right))::(_,(
MlyValue.data_description_clauses data_description_clauses,_,_))::(_,(
MlyValue.redefines_clause redefines_clause,_,_))::(_,(
MlyValue.data_name_or_filler data_name_or_filler,_,_))::(_,(
MlyValue.level_number level_number,level_numberleft as 
level_number1left,_))::rest671) => let val result=
MlyValue.data_description_entry_134((
(if (level_number = "88")
           then (case data_name_or_filler of
                  Cobol.DATA_NAME_OR_FILLER_IS_DATA_NAME(data_name)
                   => condition_names := data_name :: (!condition_names)
                | _ => ())
           else ());
           Cobol.DATA_DESCRIPTION_ENTRY_134(level_number,
                                            data_name_or_filler,
                                            redefines_clause,
                                            data_description_clauses,
                                            level_numberleft,
                                            PERIODright)
))
 in (hojfelds_NT 90,(result,level_number1left,PERIOD1right),rest671)
 end
| (509,(_,(MlyValue.data_name data_name,_,data_name1right))::(_,(_,
REDEFINES1left,_))::rest671) => let val result=
MlyValue.redefines_clause((Cobol.REDEFINES_CLAUSE(data_name)))
 in (hojfelds_NT 391,(result,REDEFINES1left,data_name1right),rest671)
 end
| (510,rest671) => let val result=MlyValue.redefines_clause((
Cobol.NO_REDEFINES_CLAUSE))
 in (hojfelds_NT 391,(result,defaultPos,defaultPos),rest671) end
| (511,(_,(MlyValue.data_description_clause data_description_clause,_,
data_description_clause1right))::(_,(MlyValue.data_description_clauses
 data_description_clauses,data_description_clauses1left,_))::rest671)
 => let val result=MlyValue.data_description_clauses((
Cobol.DATA_DESCRIPTION_CLAUSES(data_description_clauses,
                                          data_description_clause)
))
 in (hojfelds_NT 88,(result,data_description_clauses1left,
data_description_clause1right),rest671) end
| (512,rest671) => let val result=MlyValue.data_description_clauses((
Cobol.NO_DATA_DESCRIPTION_CLAUSES))
 in (hojfelds_NT 88,(result,defaultPos,defaultPos),rest671) end
| (513,(_,(MlyValue.usage_clause usage_clause,usage_clauseleft as 
usage_clause1left,usage_clauseright as usage_clause1right))::rest671)
 => let val result=MlyValue.data_description_clause((
Cobol.DATA_DESCRIPTION_CLAUSE_IS_USAGE_CLAUSE(usage_clause,
                                                         usage_clauseleft,
                                                         usage_clauseright)
))
 in (hojfelds_NT 87,(result,usage_clause1left,usage_clause1right),
rest671) end
| (514,(_,(MlyValue.sign_clause sign_clause,sign_clauseleft as 
sign_clause1left,sign_clauseright as sign_clause1right))::rest671) => 
let val result=MlyValue.data_description_clause((
let val (lt,sc) = sign_clause
                 in Cobol.DATA_DESCRIPTION_CLAUSE_IS_SIGN_CLAUSE
                     (lt,sc,sign_clauseleft,sign_clauseright)
                 end
))
 in (hojfelds_NT 87,(result,sign_clause1left,sign_clause1right),rest671
) end
| (515,(_,(MlyValue.occurs_clause occurs_clause,occurs_clauseleft as 
occurs_clause1left,occurs_clauseright as occurs_clause1right))::
rest671) => let val result=MlyValue.data_description_clause((
let val (tld, adkp, ibp) = occurs_clause
           in Cobol.DATA_DESCRIPTION_CLAUSE_IS_OCCURS_CLAUSE
                (tld, adkp, ibp, occurs_clauseleft, occurs_clauseright)
           end
))
 in (hojfelds_NT 87,(result,occurs_clause1left,occurs_clause1right),
rest671) end
| (516,(_,(MlyValue.synchronized_clause synchronized_clause,
synchronized_clauseleft as synchronized_clause1left,
synchronized_clauseright as synchronized_clause1right))::rest671) => 
let val result=MlyValue.data_description_clause((
Cobol.DATA_DESCRIPTION_CLAUSE_IS_SYNCHRONIZED_CLAUSE
             (synchronized_clause,
              synchronized_clauseleft,
              synchronized_clauseright)
))
 in (hojfelds_NT 87,(result,synchronized_clause1left,
synchronized_clause1right),rest671) end
| (517,(_,(_,justified_clauseleft as justified_clause1left,
justified_clauseright as justified_clause1right))::rest671) => let 
val result=MlyValue.data_description_clause((
Cobol.DATA_DESCRIPTION_CLAUSE_IS_JUSTIFIED_CLAUSE
             (justified_clauseleft,
              justified_clauseright)
))
 in (hojfelds_NT 87,(result,justified_clause1left,
justified_clause1right),rest671) end
| (518,(_,(_,blank_when_zero_clauseleft as blank_when_zero_clause1left
,blank_when_zero_clauseright as blank_when_zero_clause1right))::
rest671) => let val result=MlyValue.data_description_clause((
Cobol.DATA_DESCRIPTION_CLAUSE_IS_BLANK_WHEN_ZERO_CLAUSE
             (blank_when_zero_clauseleft,
              blank_when_zero_clauseright)
))
 in (hojfelds_NT 87,(result,blank_when_zero_clause1left,
blank_when_zero_clause1right),rest671) end
| (519,(_,(MlyValue.value_clause value_clause,value_clauseleft as 
value_clause1left,value_clauseright as value_clause1right))::rest671)
 => let val result=MlyValue.data_description_clause((
Cobol.DATA_DESCRIPTION_CLAUSE_IS_VALUE_CLAUSE(value_clause,
                                                         value_clauseleft,
                                                         value_clauseright)
))
 in (hojfelds_NT 87,(result,value_clause1left,value_clause1right),
rest671) end
| (520,(_,(MlyValue.picture_clause picture_clause,picture_clauseleft
 as picture_clause1left,picture_clauseright as picture_clause1right))
::rest671) => let val result=MlyValue.data_description_clause((
Cobol.DATA_DESCRIPTION_CLAUSE_IS_PICTURE_CLAUSE(picture_clause,
                                                           picture_clauseleft,
                                                           picture_clauseright)
))
 in (hojfelds_NT 87,(result,picture_clause1left,picture_clause1right),
rest671) end
| (521,(_,(MlyValue.indicator_clause indicator_clause,
indicator_clauseleft as indicator_clause1left,indicator_clauseright
 as indicator_clause1right))::rest671) => let val result=
MlyValue.data_description_clause((
Cobol.DATA_DESCRIPTION_CLAUSE_IS_INDICATOR_CLAUSE
                (indicator_clause,
                 indicator_clauseleft,
                 indicator_clauseright)
))
 in (hojfelds_NT 87,(result,indicator_clause1left,
indicator_clause1right),rest671) end
| (522,(_,(MlyValue.INTEGER INTEGER,_,INTEGER1right))::(_,(_,
indicator1left,_))::rest671) => let val result=
MlyValue.indicator_clause((INTEGER))
 in (hojfelds_NT 251,(result,indicator1left,INTEGER1right),rest671) end
| (523,(_,(_,INDICATOR1left,INDICATOR1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 252,(result,INDICATOR1left,INDICATOR1right),rest671)
 end
| (524,(_,(_,INDICATORS1left,INDICATORS1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 252,(result,INDICATORS1left,INDICATORS1right),rest671)
 end
| (525,(_,(_,INDIC1left,INDIC1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 252,(result,INDIC1left,INDIC1right),rest671) end
| (526,(_,(_,_,ZERO1right))::_::(_,(_,BLANK1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 52,(result,BLANK1left,ZERO1right),rest671) end
| (527,(_,(_,_,right1right))::(_,(_,justified1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 281,(result,justified1left,right1right),rest671) end
| (528,(_,(MlyValue.fixed_length_tables_clause 
fixed_length_tables_clause,fixed_length_tables_clause1left,
fixed_length_tables_clause1right))::rest671) => let val result=
MlyValue.occurs_clause((fixed_length_tables_clause))
 in (hojfelds_NT 329,(result,fixed_length_tables_clause1left,
fixed_length_tables_clause1right),rest671) end
| (529,(_,(MlyValue.variable_length_tables_clause 
variable_length_tables_clause,variable_length_tables_clause1left,
variable_length_tables_clause1right))::rest671) => let val result=
MlyValue.occurs_clause((variable_length_tables_clause))
 in (hojfelds_NT 329,(result,variable_length_tables_clause1left,
variable_length_tables_clause1right),rest671) end
| (530,(_,(MlyValue.indexed_by_phrase indexed_by_phrase,_,
indexed_by_phrase1right))::(_,(
MlyValue.ascending_descending_key_phrases 
ascending_descending_key_phrases,_,_))::_::(_,(MlyValue.INTEGER 
INTEGER,_,_))::(_,(_,OCCURS1left,_))::rest671) => let val result=
MlyValue.fixed_length_tables_clause((
(Cobol.FIXED_LENGTH_TABLE(INTEGER),
          ascending_descending_key_phrases,
          indexed_by_phrase)
))
 in (hojfelds_NT 211,(result,OCCURS1left,indexed_by_phrase1right),
rest671) end
| (531,(_,(MlyValue.ascending_descending_key_phrase 
ascending_descending_key_phrase,_,
ascending_descending_key_phrase1right))::(_,(
MlyValue.ascending_descending_key_phrases 
ascending_descending_key_phrases,ascending_descending_key_phrases1left
,_))::rest671) => let val result=
MlyValue.ascending_descending_key_phrases((
Cobol.ASCENDING_DESCENDING_KEY_PHRASES
               (ascending_descending_key_phrases,
                ascending_descending_key_phrase)
))
 in (hojfelds_NT 45,(result,ascending_descending_key_phrases1left,
ascending_descending_key_phrase1right),rest671) end
| (532,rest671) => let val result=
MlyValue.ascending_descending_key_phrases((
Cobol.NO_ASCENDING_DESCENDING_KEY_PHRASES))
 in (hojfelds_NT 45,(result,defaultPos,defaultPos),rest671) end
| (533,(_,(MlyValue.data_names data_names,_,data_names1right))::_::_::
(_,(MlyValue.ascending_descending ascending_descending,
ascending_descending1left,_))::rest671) => let val result=
MlyValue.ascending_descending_key_phrase((
Cobol.ASCENDING_DESCENDING_KEY_PHRASE(ascending_descending,
                                                   data_names)
))
 in (hojfelds_NT 44,(result,ascending_descending1left,data_names1right)
,rest671) end
| (534,(_,(_,ASCENDING1left,ASCENDING1right))::rest671) => let val 
result=MlyValue.ascending_descending((Cobol.ASCENDING))
 in (hojfelds_NT 43,(result,ASCENDING1left,ASCENDING1right),rest671)
 end
| (535,(_,(_,DESCENDING1left,DESCENDING1right))::rest671) => let val 
result=MlyValue.ascending_descending((Cobol.DESCENDING))
 in (hojfelds_NT 43,(result,DESCENDING1left,DESCENDING1right),rest671)
 end
| (536,(_,(MlyValue.index_names index_names,_,index_names1right))::_::
(_,(_,INDEXED1left,_))::rest671) => let val result=
MlyValue.indexed_by_phrase((Cobol.INDEXED_BY_PHRASE(index_names)))
 in (hojfelds_NT 248,(result,INDEXED1left,index_names1right),rest671)
 end
| (537,rest671) => let val result=MlyValue.indexed_by_phrase((
Cobol.NO_INDEXED_BY_PHRASE))
 in (hojfelds_NT 248,(result,defaultPos,defaultPos),rest671) end
| (538,(_,(MlyValue.indexed_by_phrase indexed_by_phrase,_,
indexed_by_phrase1right))::(_,(
MlyValue.ascending_descending_key_phrases 
ascending_descending_key_phrases,_,_))::(_,(MlyValue.data_name 
data_name,_,_))::_::_::_::(_,(MlyValue.INTEGER INTEGER2,_,_))::_::(_,(
MlyValue.INTEGER INTEGER1,_,_))::(_,(_,OCCURS1left,_))::rest671) => 
let val result=MlyValue.variable_length_tables_clause((
(Cobol.VARIABLE_LENGTH_TABLE(INTEGER1,INTEGER2,data_name),
          ascending_descending_key_phrases,
          indexed_by_phrase)
))
 in (hojfelds_NT 522,(result,OCCURS1left,indexed_by_phrase1right),
rest671) end
| (539,(_,(MlyValue.character_string character_string,_,
character_string1right))::_::(_,(_,picture1left,_))::rest671) => let 
val result=MlyValue.picture_clause((character_string))
 in (hojfelds_NT 363,(result,picture1left,character_string1right),
rest671) end
| (540,(_,(MlyValue.separate_character separate_character,_,
separate_character1right))::(_,(MlyValue.leading_trailing 
leading_trailing,_,_))::(_,(_,sign_is_opt1left,_))::rest671) => let 
val result=MlyValue.sign_clause((
(leading_trailing, separate_character)))
 in (hojfelds_NT 436,(result,sign_is_opt1left,separate_character1right)
,rest671) end
| (541,(_,(_,LEADING1left,LEADING1right))::rest671) => let val result=
MlyValue.leading_trailing((Cobol.LEADING_TRAILING_IS_LEADING))
 in (hojfelds_NT 287,(result,LEADING1left,LEADING1right),rest671) end
| (542,(_,(_,TRAILING1left,TRAILING1right))::rest671) => let val 
result=MlyValue.leading_trailing((Cobol.LEADING_TRAILING_IS_TRAILING))
 in (hojfelds_NT 287,(result,TRAILING1left,TRAILING1right),rest671) end
| (543,(_,(MlyValue.left_right left_right,_,left_right1right))::(_,(_,
synchronized1left,_))::rest671) => let val result=
MlyValue.synchronized_clause((left_right))
 in (hojfelds_NT 475,(result,synchronized1left,left_right1right),
rest671) end
| (544,(_,(_,LEFT1left,LEFT1right))::rest671) => let val result=
MlyValue.left_right((Cobol.LEFT))
 in (hojfelds_NT 289,(result,LEFT1left,LEFT1right),rest671) end
| (545,(_,(_,RIGHT1left,RIGHT1right))::rest671) => let val result=
MlyValue.left_right((Cobol.RIGHT))
 in (hojfelds_NT 289,(result,RIGHT1left,RIGHT1right),rest671) end
| (546,(_,(MlyValue.usage_specifier usage_specifier,_,
usage_specifier1right))::(_,(_,usage_is1left,_))::rest671) => let val 
result=MlyValue.usage_clause((usage_specifier))
 in (hojfelds_NT 507,(result,usage_is1left,usage_specifier1right),
rest671) end
| (547,(_,(_,DISPLAY1left,DISPLAY1right))::rest671) => let val result=
MlyValue.usage_specifier((Cobol.USAGE_IS_DISPLAY))
 in (hojfelds_NT 509,(result,DISPLAY1left,DISPLAY1right),rest671) end
| (548,(_,(_,INDEX1left,INDEX1right))::rest671) => let val result=
MlyValue.usage_specifier((Cobol.USAGE_IS_INDEX))
 in (hojfelds_NT 509,(result,INDEX1left,INDEX1right),rest671) end
| (549,(_,(_,COMPUTATIONAL31left,COMPUTATIONAL31right))::rest671) => 
let val result=MlyValue.usage_specifier((Cobol.USAGE_IS_COMP3))
 in (hojfelds_NT 509,(result,COMPUTATIONAL31left,COMPUTATIONAL31right),
rest671) end
| (550,(_,(_,COMP31left,COMP31right))::rest671) => let val result=
MlyValue.usage_specifier((Cobol.USAGE_IS_COMP3))
 in (hojfelds_NT 509,(result,COMP31left,COMP31right),rest671) end
| (551,(_,(_,COMPUTATIONAL41left,COMPUTATIONAL41right))::rest671) => 
let val result=MlyValue.usage_specifier((Cobol.USAGE_IS_COMP4))
 in (hojfelds_NT 509,(result,COMPUTATIONAL41left,COMPUTATIONAL41right),
rest671) end
| (552,(_,(_,COMP41left,COMP41right))::rest671) => let val result=
MlyValue.usage_specifier((Cobol.USAGE_IS_COMP4))
 in (hojfelds_NT 509,(result,COMP41left,COMP41right),rest671) end
| (553,(_,(_,COMPUTATIONAL1left,COMPUTATIONAL1right))::rest671) => 
let val result=MlyValue.usage_specifier((Cobol.USAGE_IS_COMP))
 in (hojfelds_NT 509,(result,COMPUTATIONAL1left,COMPUTATIONAL1right),
rest671) end
| (554,(_,(_,COMP1left,COMP1right))::rest671) => let val result=
MlyValue.usage_specifier((Cobol.USAGE_IS_COMP))
 in (hojfelds_NT 509,(result,COMP1left,COMP1right),rest671) end
| (555,(_,(MlyValue.literal_ranges literal_ranges,_,
literal_ranges1right))::(_,(_,value_is1left,_))::rest671) => let val 
result=MlyValue.value_clause((literal_ranges))
 in (hojfelds_NT 516,(result,value_is1left,literal_ranges1right),
rest671) end
| (556,(_,(_,_,is1right))::(_,(_,VALUE1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 517,(result,VALUE1left,is1right),rest671) end
| (557,(_,(_,_,are1right))::(_,(_,VALUES1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 517,(result,VALUES1left,are1right),rest671) end
| (558,(_,(_,_,PERIODright as PERIOD1right))::(_,(
MlyValue.through_data_name through_data_name,_,_))::(_,(
MlyValue.data_name data_name2,_,_))::_::(_,(MlyValue.data_name 
data_name1,_,_))::(_,(MlyValue.level_number level_number,
level_numberleft as level_number1left,_))::rest671) => let val result=
MlyValue.renames_clause((
Cobol.RENAMES_CLAUSE(level_number,
                                data_name1,
                                data_name2,
                                through_data_name,
                                level_numberleft,
                                PERIODright)
))
 in (hojfelds_NT 401,(result,level_number1left,PERIOD1right),rest671)
 end
| (559,(_,(MlyValue.data_name data_name,_,data_name1right))::(_,(_,
through1left,_))::rest671) => let val result=
MlyValue.through_data_name((Cobol.THROUGH_DATA_NAME(data_name)))
 in (hojfelds_NT 492,(result,through1left,data_name1right),rest671) end
| (560,rest671) => let val result=MlyValue.through_data_name((
Cobol.NOT_THROUGH_DATA_NAME))
 in (hojfelds_NT 492,(result,defaultPos,defaultPos),rest671) end
| (561,(_,(MlyValue.record_description_entries_opt 
record_description_entries_opt,_,record_description_entries_optright
 as record_description_entries_opt1right))::_::_::(_,(_,
WORKINGSTORAGEleft as WORKINGSTORAGE1left,_))::rest671) => let val 
result=MlyValue.working_storage_section((
Cobol.WORKINGSTORAGE_SECTION(record_description_entries_opt,
                                        WORKINGSTORAGEleft,record_description_entries_optright)
))
 in (hojfelds_NT 529,(result,WORKINGSTORAGE1left,
record_description_entries_opt1right),rest671) end
| (562,rest671) => let val result=MlyValue.working_storage_section((
Cobol.NO_WORKINGSTORAGE_SECTION))
 in (hojfelds_NT 529,(result,defaultPos,defaultPos),rest671) end
| (563,(_,(MlyValue.record_description_entries_opt 
record_description_entries_opt,_,record_description_entries_optright
 as record_description_entries_opt1right))::_::_::(_,(_,LINKAGEleft
 as LINKAGE1left,_))::rest671) => let val result=
MlyValue.linkage_section((
Cobol.LINKAGE_SECTION(record_description_entries_opt,
                                 LINKAGEleft,record_description_entries_optright)
))
 in (hojfelds_NT 297,(result,LINKAGE1left,
record_description_entries_opt1right),rest671) end
| (564,rest671) => let val result=MlyValue.linkage_section((
Cobol.NO_LINKAGE_SECTION))
 in (hojfelds_NT 297,(result,defaultPos,defaultPos),rest671) end
| (565,(_,(MlyValue.sections sections,_,sectionsright as 
sections1right))::(_,(MlyValue.declaratives_section 
declaratives_section,_,_))::_::(_,(MlyValue.using_clause using_clause,
_,_))::_::(_,(_,PROCEDUREleft as PROCEDURE1left,_))::rest671) => let 
val result=MlyValue.procedure_division((
Cobol.PROCEDURE_DIVISION_FORMAT_1_DECLARATIVES(using_clause,
                                                          declaratives_section,
                                                          sections,
                                                          PROCEDUREleft,
                                                          sectionsright)
))
 in (hojfelds_NT 367,(result,PROCEDURE1left,sections1right),rest671)
 end
| (566,(_,(MlyValue.sections sections,_,sectionsright as 
sections1right))::_::(_,(MlyValue.using_clause using_clause,_,_))::_::
(_,(_,PROCEDUREleft as PROCEDURE1left,_))::rest671) => let val result=
MlyValue.procedure_division((
Cobol.PROCEDURE_DIVISION_FORMAT_1_NO_DECLARATIVES(using_clause,
                                                             sections,
                                                             PROCEDUREleft,
                                                             sectionsright)
))
 in (hojfelds_NT 367,(result,PROCEDURE1left,sections1right),rest671)
 end
| (567,(_,(MlyValue.paragraphs paragraphs,_,paragraphsright as 
paragraphs1right))::_::(_,(MlyValue.using_clause using_clause,_,_))::_
::(_,(_,PROCEDUREleft as PROCEDURE1left,_))::rest671) => let val 
result=MlyValue.procedure_division((
Cobol.PROCEDURE_DIVISION_FORMAT_2(using_clause,
                                             paragraphs,
                                             PROCEDUREleft,
                                             paragraphsright)
))
 in (hojfelds_NT 367,(result,PROCEDURE1left,paragraphs1right),rest671)
 end
| (568,rest671) => let val result=MlyValue.procedure_division((
Cobol.EMPTY_PROCEDURE_DIVISION))
 in (hojfelds_NT 367,(result,defaultPos,defaultPos),rest671) end
| (569,(_,(_,_,data_names1right))::(_,(_,USING1left,_))::rest671) => 
let val result=MlyValue.using_clause(())
 in (hojfelds_NT 512,(result,USING1left,data_names1right),rest671) end
| (570,rest671) => let val result=MlyValue.using_clause(())
 in (hojfelds_NT 512,(result,defaultPos,defaultPos),rest671) end
| (571,(_,(_,_,PERIOD2right))::_::_::_::_::(_,(_,DECLARATIVES1left,_))
::rest671) => let val result=MlyValue.declaratives_section(())
 in (hojfelds_NT 118,(result,DECLARATIVES1left,PERIOD2right),rest671)
 end
| (572,(_,(_,_,PERIOD2right))::_::_::_::_::(_,(_,section_name1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 119,(result,section_name1left,PERIOD2right),rest671)
 end
| (573,(_,(_,_,declaratives_sections1right))::_::_::_::_::_::(_,(_,
section_name1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 119,(result,section_name1left,
declaratives_sections1right),rest671) end
| (574,(_,(_,_,declaratives_paragraphs1right))::_::_::_::_::_::(_,(_,
section_name1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 119,(result,section_name1left,
declaratives_paragraphs1right),rest671) end
| (575,(_,(_,_,declaratives_paragraphs1right))::_::_::(_,(_,
paragraph_name1left,_))::rest671) => let val result=MlyValue.ntVOID(()
)
 in (hojfelds_NT 117,(result,paragraph_name1left,
declaratives_paragraphs1right),rest671) end
| (576,(_,(_,_,declaratives_sections1right))::_::_::(_,(_,
paragraph_name1left,_))::rest671) => let val result=MlyValue.ntVOID(()
)
 in (hojfelds_NT 117,(result,paragraph_name1left,
declaratives_sections1right),rest671) end
| (577,(_,(_,_,sentences1right))::_::(_,(_,paragraph_name1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 117,(result,paragraph_name1left,sentences1right),
rest671) end
| (578,(_,(_,_,PERIOD1right))::(_,(MlyValue.segment_number_opt 
segment_number_opt,_,_))::_::(_,(MlyValue.section_name section_name,
section_name1left,_))::rest671) => let val result=MlyValue.sections((
Cobol.NO_BODY_SECTION(section_name,segment_number_opt)))
 in (hojfelds_NT 424,(result,section_name1left,PERIOD1right),rest671)
 end
| (579,(_,(MlyValue.sections sections,_,sections1right))::_::(_,(
MlyValue.segment_number_opt segment_number_opt,_,_))::_::(_,(
MlyValue.section_name section_name,section_name1left,_))::rest671) => 
let val result=MlyValue.sections((
Cobol.NO_BODY_SECTION_FOLLOWED_BY_SECTION(section_name,
                                                     segment_number_opt,
                                                     sections)
))
 in (hojfelds_NT 424,(result,section_name1left,sections1right),rest671)
 end
| (580,(_,(MlyValue.paragraphs_and_sections paragraphs_and_sections,_,
paragraphs_and_sections1right))::_::(_,(MlyValue.segment_number_opt 
segment_number_opt,_,_))::_::(_,(MlyValue.section_name section_name,
section_name1left,_))::rest671) => let val result=MlyValue.sections((
Cobol.SECTION(section_name,
                         segment_number_opt,
                         paragraphs_and_sections)
))
 in (hojfelds_NT 424,(result,section_name1left,
paragraphs_and_sections1right),rest671) end
| (581,(_,(MlyValue.paragraphs_and_sections paragraphs_and_sections,_,
paragraphs_and_sections1right))::(_,(MlyValue.paragraph paragraph,
paragraph1left,_))::rest671) => let val result=
MlyValue.paragraphs_and_sections((
Cobol.SEVERAL_PARAGRAPHS_AND_SECTIONS(paragraph,
                                                 paragraphs_and_sections)
))
 in (hojfelds_NT 359,(result,paragraph1left,
paragraphs_and_sections1right),rest671) end
| (582,(_,(MlyValue.sections sections,_,sections1right))::(_,(
MlyValue.paragraph paragraph,paragraph1left,_))::rest671) => let val 
result=MlyValue.paragraphs_and_sections((
Cobol.SINGLE_PARAGRAPH_AND_SECTIONS(paragraph,sections)))
 in (hojfelds_NT 359,(result,paragraph1left,sections1right),rest671)
 end
| (583,(_,(MlyValue.paragraph paragraph,paragraph1left,paragraph1right
))::rest671) => let val result=MlyValue.paragraphs_and_sections((
Cobol.SINGLE_PARAGRAPH(paragraph)))
 in (hojfelds_NT 359,(result,paragraph1left,paragraph1right),rest671)
 end
| (584,(_,(MlyValue.paragraph paragraph,_,paragraph1right))::(_,(
MlyValue.paragraphs paragraphs,paragraphs1left,_))::rest671) => let 
val result=MlyValue.paragraphs((
Cobol.SEVERAL_PARAGRAPHS(paragraphs,paragraph)))
 in (hojfelds_NT 358,(result,paragraphs1left,paragraph1right),rest671)
 end
| (585,(_,(MlyValue.paragraph paragraph,paragraph1left,paragraph1right
))::rest671) => let val result=MlyValue.paragraphs((
Cobol.ONE_PARAGRAPH(paragraph)))
 in (hojfelds_NT 358,(result,paragraph1left,paragraph1right),rest671)
 end
| (586,(_,(MlyValue.sentences sentences,_,sentences1right))::_::(_,(
MlyValue.paragraph_name paragraph_name,paragraph_name1left,_))::
rest671) => let val result=MlyValue.paragraph((
Cobol.PARAGRAPH(paragraph_name,sentences)))
 in (hojfelds_NT 356,(result,paragraph_name1left,sentences1right),
rest671) end
| (587,(_,(MlyValue.sentence sentence,_,sentence1right))::(_,(
MlyValue.sentences sentences,sentences1left,_))::rest671) => let val 
result=MlyValue.sentences((Cobol.SENTENCES(sentences,sentence)))
 in (hojfelds_NT 430,(result,sentences1left,sentence1right),rest671)
 end
| (588,rest671) => let val result=MlyValue.sentences((
Cobol.NO_SENTENCES))
 in (hojfelds_NT 430,(result,defaultPos,defaultPos),rest671) end
| (589,(_,(_,_,PERIOD1right))::(_,(MlyValue.statements statements,
statements1left,_))::rest671) => let val result=MlyValue.sentence((
Cobol.SENTENCE(statements)))
 in (hojfelds_NT 429,(result,statements1left,PERIOD1right),rest671) end
| (590,(_,(MlyValue.statement statement,_,statement1right))::(_,(
MlyValue.statements statements,statements1left,_))::rest671) => let 
val result=MlyValue.statements((
Cobol.SEVERAL_STATEMENTS(statements,statement)))
 in (hojfelds_NT 459,(result,statements1left,statement1right),rest671)
 end
| (591,(_,(MlyValue.statement statement,statement1left,statement1right
))::rest671) => let val result=MlyValue.statements((
Cobol.ONE_STATEMENT(statement)))
 in (hojfelds_NT 459,(result,statement1left,statement1right),rest671)
 end
| (592,(_,(MlyValue.statements statements,statements1left,
statements1right))::rest671) => let val result=
MlyValue.imperative_statement((statements))
 in (hojfelds_NT 241,(result,statements1left,statements1right),rest671)
 end
| (593,(_,(MlyValue.INTEGER INTEGER,INTEGERleft as INTEGER1left,
INTEGERright as INTEGER1right))::rest671) => let val result=
MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_INTEGER(INTEGER,INTEGERleft,INTEGERright)
))
 in (hojfelds_NT 476,(result,INTEGER1left,INTEGER1right),rest671) end
| (594,(_,(MlyValue.DECIMALNUMBER DECIMALNUMBER,DECIMALNUMBERleft as 
DECIMALNUMBER1left,DECIMALNUMBERright as DECIMALNUMBER1right))::
rest671) => let val result=MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_DECIMALNUMBER
                (DECIMALNUMBER,DECIMALNUMBERleft,DECIMALNUMBERright)
))
 in (hojfelds_NT 476,(result,DECIMALNUMBER1left,DECIMALNUMBER1right),
rest671) end
| (595,(_,(MlyValue.NONNUMERICLITERAL NONNUMERICLITERAL,
NONNUMERICLITERALleft as NONNUMERICLITERAL1left,NONNUMERICLITERALright
 as NONNUMERICLITERAL1right))::rest671) => let val result=
MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_NONNUMERICLITERAL(NONNUMERICLITERAL,
                                                            NONNUMERICLITERALleft,
                                                            NONNUMERICLITERALright)
))
 in (hojfelds_NT 476,(result,NONNUMERICLITERAL1left,
NONNUMERICLITERAL1right),rest671) end
| (596,(_,(MlyValue.BOOLEANLITERAL BOOLEANLITERAL,BOOLEANLITERALleft
 as BOOLEANLITERAL1left,BOOLEANLITERALright as BOOLEANLITERAL1right))
::rest671) => let val result=MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_BOOLEANLITERAL
                (BOOLEANLITERAL,BOOLEANLITERALleft,BOOLEANLITERALright)
))
 in (hojfelds_NT 476,(result,BOOLEANLITERAL1left,BOOLEANLITERAL1right),
rest671) end
| (597,(_,(MlyValue.figurative_constant figurative_constant,
figurative_constantleft as figurative_constant1left,
figurative_constantright as figurative_constant1right))::rest671) => 
let val result=MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_FIGURATIVE_CONSTANT
                 (figurative_constant,figurative_constantleft,figurative_constantright)
))
 in (hojfelds_NT 476,(result,figurative_constant1left,
figurative_constant1right),rest671) end
| (598,(_,(MlyValue.identifier identifier,identifier1left,
identifier1right))::rest671) => let val result=
MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_IDENTIFIER(identifier)))
 in (hojfelds_NT 476,(result,identifier1left,identifier1right),rest671)
 end
| (599,(_,(MlyValue.arithmetic_expression arithmetic_expression,_,
arithmetic_expression1right))::(_,(_,PLUSSYMBOL1left,_))::rest671) => 
let val result=MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_PLUS_SIGN(arithmetic_expression)))
 in (hojfelds_NT 476,(result,PLUSSYMBOL1left,
arithmetic_expression1right),rest671) end
| (600,(_,(MlyValue.arithmetic_expression arithmetic_expression,_,
arithmetic_expression1right))::(_,(_,MINUSSYMBOL1left,_))::rest671)
 => let val result=MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_MINUS_SIGN(arithmetic_expression)))
 in (hojfelds_NT 476,(result,MINUSSYMBOL1left,
arithmetic_expression1right),rest671) end
| (601,(_,(MlyValue.arithmetic_expression arithmetic_expression2,_,
arithmetic_expression2right))::_::(_,(MlyValue.arithmetic_expression 
arithmetic_expression1,arithmetic_expression1left,_))::rest671) => 
let val result=MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_EXPONENTIATION
            (arithmetic_expression1,arithmetic_expression2)
))
 in (hojfelds_NT 476,(result,arithmetic_expression1left,
arithmetic_expression2right),rest671) end
| (602,(_,(MlyValue.arithmetic_expression arithmetic_expression2,_,
arithmetic_expression2right))::_::(_,(MlyValue.arithmetic_expression 
arithmetic_expression1,arithmetic_expression1left,_))::rest671) => 
let val result=MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_MULTIPLY
            (arithmetic_expression1,arithmetic_expression2)
))
 in (hojfelds_NT 476,(result,arithmetic_expression1left,
arithmetic_expression2right),rest671) end
| (603,(_,(MlyValue.arithmetic_expression arithmetic_expression2,_,
arithmetic_expression2right))::_::(_,(MlyValue.arithmetic_expression 
arithmetic_expression1,arithmetic_expression1left,_))::rest671) => 
let val result=MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_DIVIDE
            (arithmetic_expression1,arithmetic_expression2)
))
 in (hojfelds_NT 476,(result,arithmetic_expression1left,
arithmetic_expression2right),rest671) end
| (604,(_,(MlyValue.arithmetic_expression arithmetic_expression2,_,
arithmetic_expression2right))::_::(_,(MlyValue.arithmetic_expression 
arithmetic_expression1,arithmetic_expression1left,_))::rest671) => 
let val result=MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_ADD
            (arithmetic_expression1,arithmetic_expression2)
))
 in (hojfelds_NT 476,(result,arithmetic_expression1left,
arithmetic_expression2right),rest671) end
| (605,(_,(MlyValue.arithmetic_expression arithmetic_expression2,_,
arithmetic_expression2right))::_::(_,(MlyValue.arithmetic_expression 
arithmetic_expression1,arithmetic_expression1left,_))::rest671) => 
let val result=MlyValue.arithmetic_expression((
Cobol.ARITHMETIC_EXPRESSION_IS_SUBTRACT
            (arithmetic_expression1,arithmetic_expression2)
))
 in (hojfelds_NT 476,(result,arithmetic_expression1left,
arithmetic_expression2right),rest671) end
| (606,(_,(_,_,RPAR1right))::(_,(MlyValue.arithmetic_expression 
arithmetic_expression,_,_))::(_,(_,LPAR1left,_))::rest671) => let val 
result=MlyValue.arithmetic_expression((arithmetic_expression))
 in (hojfelds_NT 476,(result,LPAR1left,RPAR1right),rest671) end
| (607,(_,(MlyValue.expression expression,expression1left,
expression1right))::rest671) => raise Hojfeld "607" 
(*
   let val result=
MlyValue.conditional_expression((convertCE(expression)))
 in (hojfelds_NT 477,(result,expression1left,expression1right),rest671)
 end
*)
| (608,(_,(MlyValue.arithmetic_expression arithmetic_expression,
arithmetic_expressionleft as arithmetic_expression1left,
arithmetic_expressionright as arithmetic_expression1right))::rest671)
 => let val result=MlyValue.expression((
CE_AE(arithmetic_expression,
                 arithmetic_expressionleft,
                 arithmetic_expressionright)
))
 in (hojfelds_NT 42,(result,arithmetic_expression1left,
arithmetic_expression1right),rest671) end
| (609,(_,(MlyValue.arithmetic_expression arithmetic_expression,_,
arithmetic_expressionright as arithmetic_expression1right))::(_,(
MlyValue.relational_operator relational_operator,
relational_operatorleft as relational_operator1left,_))::rest671) => 
let val result=MlyValue.expression((
CE_SINGLE_REL(relational_operator,
                         arithmetic_expression,
                         relational_operatorleft,
                         arithmetic_expressionright)
))
 in (hojfelds_NT 42,(result,relational_operator1left,
arithmetic_expression1right),rest671) end
| (610,(_,(MlyValue.arithmetic_expression arithmetic_expression2,_,
arithmetic_expression2right))::(_,(MlyValue.relational_operator 
relational_operator,_,_))::(_,(MlyValue.is_not is_not,_,_))::(_,(
MlyValue.arithmetic_expression arithmetic_expression1,
arithmetic_expression1left,_))::rest671) => let val result=
MlyValue.expression((
CE_REL(arithmetic_expression1,
                  is_not,
                  relational_operator,
                  arithmetic_expression2,
                  arithmetic_expression1left,
                  arithmetic_expression2right)
))
 in (hojfelds_NT 42,(result,arithmetic_expression1left,
arithmetic_expression2right),rest671) end
| (611,(_,(MlyValue.data_class data_class,_,data_class1right))::(_,(
MlyValue.is_not is_not,_,_))::(_,(MlyValue.arithmetic_expression 
arithmetic_expression,arithmetic_expression1left,_))::rest671) => let 
val result=MlyValue.expression((
CE_DC(arithmetic_expression,is_not,data_class)))
 in (hojfelds_NT 42,(result,arithmetic_expression1left,data_class1right
),rest671) end
| (612,(_,(MlyValue.sign_specification sign_specification,_,
sign_specification1right))::(_,(MlyValue.is_not is_not,_,_))::(_,(
MlyValue.arithmetic_expression arithmetic_expression,
arithmetic_expression1left,_))::rest671) => let val result=
MlyValue.expression((
CE_SIGN(arithmetic_expression,is_not,sign_specification)))
 in (hojfelds_NT 42,(result,arithmetic_expression1left,
sign_specification1right),rest671) end
| (613,(_,(_,TRUEleft as TRUE1left,TRUEright as TRUE1right))::rest671)
 => let val result=MlyValue.expression((CE_TRUE(TRUEleft,TRUEright)))
 in (hojfelds_NT 42,(result,TRUE1left,TRUE1right),rest671) end
| (614,(_,(_,FALSEleft as FALSE1left,FALSEright as FALSE1right))::
rest671) => let val result=MlyValue.expression((
CE_FALSE(FALSEleft,FALSEright)))
 in (hojfelds_NT 42,(result,FALSE1left,FALSE1right),rest671) end
| (615,(_,(_,environment_name_2left as environment_name_21left,
environment_name_2right as environment_name_21right))::rest671) => 
let val result=MlyValue.expression((
CE_SWITCH(environment_name_2left,environment_name_2right)))
 in (hojfelds_NT 42,(result,environment_name_21left,
environment_name_21right),rest671) end
| (616,(_,(MlyValue.expression expression,_,expression1right))::(_,(_,
NOT1left,_))::rest671) => let val result=MlyValue.expression((
CE_NOT(expression)))
 in (hojfelds_NT 42,(result,NOT1left,expression1right),rest671) end
| (617,(_,(MlyValue.expression expression2,_,expression2right))::_::(_
,(MlyValue.expression expression1,expression1left,_))::rest671) => 
let val result=MlyValue.expression((CE_AND(expression1,expression2)))
 in (hojfelds_NT 42,(result,expression1left,expression2right),rest671)
 end
| (618,(_,(MlyValue.expression expression2,_,expression2right))::_::(_
,(MlyValue.expression expression1,expression1left,_))::rest671) => 
let val result=MlyValue.expression((CE_OR(expression1,expression2)))
 in (hojfelds_NT 42,(result,expression1left,expression2right),rest671)
 end
| (619,(_,(_,_,RPAR1right))::(_,(MlyValue.expression expression,_,_))
::(_,(_,LPAR1left,_))::rest671) => let val result=MlyValue.expression(
(expression))
 in (hojfelds_NT 42,(result,LPAR1left,RPAR1right),rest671) end
| (620,(_,(_,NUMERIC1left,NUMERIC1right))::rest671) => let val result=
MlyValue.data_class((Cobol.DATA_CLASS_IS_NUMERIC))
 in (hojfelds_NT 122,(result,NUMERIC1left,NUMERIC1right),rest671) end
| (621,(_,(_,ALPHABETIC1left,ALPHABETIC1right))::rest671) => let val 
result=MlyValue.data_class((Cobol.DATA_CLASS_IS_ALPHABETIC))
 in (hojfelds_NT 122,(result,ALPHABETIC1left,ALPHABETIC1right),rest671)
 end
| (622,(_,(_,_,than1right))::(_,(_,GREATER1left,_))::rest671) => let 
val result=MlyValue.relational_operator((Cobol.GREATER))
 in (hojfelds_NT 121,(result,GREATER1left,than1right),rest671) end
| (623,(_,(_,GREATERSYMBOL1left,GREATERSYMBOL1right))::rest671) => 
let val result=MlyValue.relational_operator((Cobol.GREATER))
 in (hojfelds_NT 121,(result,GREATERSYMBOL1left,GREATERSYMBOL1right),
rest671) end
| (624,(_,(_,_,than1right))::(_,(_,LESS1left,_))::rest671) => let val 
result=MlyValue.relational_operator((Cobol.LESS))
 in (hojfelds_NT 121,(result,LESS1left,than1right),rest671) end
| (625,(_,(_,LESSSYMBOL1left,LESSSYMBOL1right))::rest671) => let val 
result=MlyValue.relational_operator((Cobol.LESS))
 in (hojfelds_NT 121,(result,LESSSYMBOL1left,LESSSYMBOL1right),rest671)
 end
| (626,(_,(_,_,to1right))::(_,(_,EQUAL1left,_))::rest671) => let val 
result=MlyValue.relational_operator((Cobol.EQUAL))
 in (hojfelds_NT 121,(result,EQUAL1left,to1right),rest671) end
| (627,(_,(_,EQSYMBOL1left,EQSYMBOL1right))::rest671) => let val 
result=MlyValue.relational_operator((Cobol.EQUAL))
 in (hojfelds_NT 121,(result,EQSYMBOL1left,EQSYMBOL1right),rest671) end
| (628,(_,(_,POSITIVE1left,POSITIVE1right))::rest671) => let val 
result=MlyValue.sign_specification((
Cobol.SIGN_SPECIFICATION_IS_POSITIVE))
 in (hojfelds_NT 123,(result,POSITIVE1left,POSITIVE1right),rest671) end
| (629,(_,(_,NEGATIVE1left,NEGATIVE1right))::rest671) => let val 
result=MlyValue.sign_specification((
Cobol.SIGN_SPECIFICATION_IS_NEGATIVE))
 in (hojfelds_NT 123,(result,NEGATIVE1left,NEGATIVE1right),rest671) end
| (630,(_,(_,ZERO1left,ZERO1right))::rest671) => let val result=
MlyValue.sign_specification((Cobol.SIGN_SPECIFICATION_IS_ZERO))
 in (hojfelds_NT 123,(result,ZERO1left,ZERO1right),rest671) end
| (631,(_,(MlyValue.accept_statement accept_statement,
accept_statement1left,accept_statement1right))::rest671) => let val 
result=MlyValue.statement((Cobol.ACCEPT_STATEMENT(accept_statement)))
 in (hojfelds_NT 458,(result,accept_statement1left,
accept_statement1right),rest671) end
| (632,(_,(MlyValue.add_statement add_statement,add_statement1left,
add_statement1right))::rest671) => let val result=MlyValue.statement((
Cobol.ADD_OR_SUBTRACT_STATEMENT(add_statement)))
 in (hojfelds_NT 458,(result,add_statement1left,add_statement1right),
rest671) end
| (633,(_,(_,alter_statement1left,alter_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,alter_statement1left,alter_statement1right
),rest671) end
| (634,(_,(_,call_statement1left,call_statement1right))::rest671) => 
let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,call_statement1left,call_statement1right),
rest671) end
| (635,(_,(_,cancel_statement1left,cancel_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,cancel_statement1left,
cancel_statement1right),rest671) end
| (636,(_,(_,close_statement1left,close_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,close_statement1left,close_statement1right
),rest671) end
| (637,(_,(MlyValue.compute_statement compute_statement,
compute_statement1left,compute_statement1right))::rest671) => let val 
result=MlyValue.statement((Cobol.COMPUTE_STATEMENT(compute_statement))
)
 in (hojfelds_NT 458,(result,compute_statement1left,
compute_statement1right),rest671) end
| (638,(_,(_,delete_statement1left,delete_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,delete_statement1left,
delete_statement1right),rest671) end
| (639,(_,(_,display_statement1left,display_statement1right))::rest671
) => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,display_statement1left,
display_statement1right),rest671) end
| (640,(_,(MlyValue.divide_statement divide_statement,
divide_statement1left,divide_statement1right))::rest671) => let val 
result=MlyValue.statement((Cobol.DIVIDE_STATEMENT(divide_statement)))
 in (hojfelds_NT 458,(result,divide_statement1left,
divide_statement1right),rest671) end
| (641,(_,(_,enter_statement1left,enter_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,enter_statement1left,enter_statement1right
),rest671) end
| (642,(_,(_,exit_statement1left,exit_statement1right))::rest671) => 
let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,exit_statement1left,exit_statement1right),
rest671) end
| (643,(_,(_,exit_program_statement1left,exit_program_statement1right)
)::rest671) => let val result=MlyValue.statement((
Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,exit_program_statement1left,
exit_program_statement1right),rest671) end
| (644,(_,(_,goback_statement1left,goback_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,goback_statement1left,
goback_statement1right),rest671) end
| (645,(_,(_,go_to_statement1left,go_to_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,go_to_statement1left,go_to_statement1right
),rest671) end
| (646,(_,(MlyValue.if_statement if_statement,if_statement1left,
if_statement1right))::rest671) => let val result=MlyValue.statement((
Cobol.IF_STATEMENT(if_statement)))
 in (hojfelds_NT 458,(result,if_statement1left,if_statement1right),
rest671) end
| (647,(_,(_,initialize_statement1left,initialize_statement1right))::
rest671) => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,initialize_statement1left,
initialize_statement1right),rest671) end
| (648,(_,(_,inspect_statement1left,inspect_statement1right))::rest671
) => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,inspect_statement1left,
inspect_statement1right),rest671) end
| (649,(_,(_,merge_statement1left,merge_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,merge_statement1left,merge_statement1right
),rest671) end
| (650,(_,(MlyValue.move_statement move_statement,move_statement1left,
move_statement1right))::rest671) => let val result=MlyValue.statement(
(Cobol.MOVE_STATEMENT(move_statement)))
 in (hojfelds_NT 458,(result,move_statement1left,move_statement1right),
rest671) end
| (651,(_,(MlyValue.multiply_statement multiply_statement,
multiply_statement1left,multiply_statement1right))::rest671) => let 
val result=MlyValue.statement((
Cobol.MULTIPLY_STATEMENT(multiply_statement)))
 in (hojfelds_NT 458,(result,multiply_statement1left,
multiply_statement1right),rest671) end
| (652,(_,(_,open_statement1left,open_statement1right))::rest671) => 
let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,open_statement1left,open_statement1right),
rest671) end
| (653,(_,(_,perform_statement1left,perform_statement1right))::rest671
) => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,perform_statement1left,
perform_statement1right),rest671) end
| (654,(_,(_,read_statement1left,read_statement1right))::rest671) => 
let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,read_statement1left,read_statement1right),
rest671) end
| (655,(_,(_,release_statement1left,release_statement1right))::rest671
) => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,release_statement1left,
release_statement1right),rest671) end
| (656,(_,(_,return_statement1left,return_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,return_statement1left,
return_statement1right),rest671) end
| (657,(_,(_,rewrite_statement1left,rewrite_statement1right))::rest671
) => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,rewrite_statement1left,
rewrite_statement1right),rest671) end
| (658,(_,(_,search_statement1left,search_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,search_statement1left,
search_statement1right),rest671) end
| (659,(_,(_,set_statement1left,set_statement1right))::rest671) => 
let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,set_statement1left,set_statement1right),
rest671) end
| (660,(_,(_,sort_statement1left,sort_statement1right))::rest671) => 
let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,sort_statement1left,sort_statement1right),
rest671) end
| (661,(_,(_,start_statement1left,start_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,start_statement1left,start_statement1right
),rest671) end
| (662,(_,(_,stop_statement1left,stop_statement1right))::rest671) => 
let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,stop_statement1left,stop_statement1right),
rest671) end
| (663,(_,(_,string_statement1left,string_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,string_statement1left,
string_statement1right),rest671) end
| (664,(_,(MlyValue.subtract_statement subtract_statement,
subtract_statement1left,subtract_statement1right))::rest671) => let 
val result=MlyValue.statement((
Cobol.ADD_OR_SUBTRACT_STATEMENT(subtract_statement)))
 in (hojfelds_NT 458,(result,subtract_statement1left,
subtract_statement1right),rest671) end
| (665,(_,(_,unstring_statement1left,unstring_statement1right))::
rest671) => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,unstring_statement1left,
unstring_statement1right),rest671) end
| (666,(_,(_,use_statement1left,use_statement1right))::rest671) => 
let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,use_statement1left,use_statement1right),
rest671) end
| (667,(_,(_,write_statement1left,write_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,write_statement1left,write_statement1right
),rest671) end
| (668,(_,(_,copy_statement1left,copy_statement1right))::rest671) => 
let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,copy_statement1left,copy_statement1right),
rest671) end
| (669,(_,(_,eject_statement1left,eject_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,eject_statement1left,eject_statement1right
),rest671) end
| (670,(_,(_,exhibit_statement1left,exhibit_statement1right))::rest671
) => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,exhibit_statement1left,
exhibit_statement1right),rest671) end
| (671,(_,(_,trace_statement1left,trace_statement1right))::rest671)
 => let val result=MlyValue.statement((Cobol.OTHER_STATEMENT))
 in (hojfelds_NT 458,(result,trace_statement1left,trace_statement1right
),rest671) end
| (672,(_,(MlyValue.annotation_statement annotation_statement,
annotation_statement1left,annotation_statement1right))::rest671) => 
let val result=MlyValue.statement((
Cobol.ANNOTATION_STATEMENT(annotation_statement)))
 in (hojfelds_NT 458,(result,annotation_statement1left,
annotation_statement1right),rest671) end
| (673,(_,(MlyValue.identifier_or_literal identifier_or_literal,
identifier_or_literal1left,identifier_or_literal1right))::rest671) => 
let val result=MlyValue.identifier_or_literal_or_ddt((
Cobol.IDENTIFIER_OR_LITERAL_OR_DDT_IS_IDENTIFIER_OR_LITERAL(identifier_or_literal)
))
 in (hojfelds_NT 544,(result,identifier_or_literal1left,
identifier_or_literal1right),rest671) end
| (674,(_,(MlyValue.date_day_time date_day_time,date_day_time1left,
date_day_time1right))::rest671) => let val result=
MlyValue.identifier_or_literal_or_ddt((
Cobol.IDENTIFIER_OR_LITERAL_OR_DDT_IS_DDT(date_day_time)))
 in (hojfelds_NT 544,(result,date_day_time1left,date_day_time1right),
rest671) end
| (675,(_,(MlyValue.identifier identifier,_,identifierright as 
identifier1right))::_::(_,(MlyValue.ts2k ts2k,_,_))::_::(_,(
MlyValue.identifier_or_literal_or_ddt identifier_or_literal_or_ddt,_,_
))::_::(_,(_,TS2Kleft as TS2K1left,_))::rest671) => let val result=
MlyValue.annotation_statement((
Cobol.TS2K_COERCE(identifier_or_literal_or_ddt, ts2k, TS2Kleft,identifierright,identifier)
))
 in (hojfelds_NT 542,(result,TS2K1left,identifier1right),rest671) end
| (676,(_,(MlyValue.ts2k ts2k,_,ts2kright as ts2k1right))::_::(_,(
MlyValue.identifier_or_literal_or_ddt identifier_or_literal_or_ddt,_,_
))::_::(_,(_,TS2Kleft as TS2K1left,_))::rest671) => let val result=
MlyValue.annotation_statement((
Cobol.TS2K_ASSUME(identifier_or_literal_or_ddt,ts2k,TS2Kleft,ts2kright)
))
 in (hojfelds_NT 542,(result,TS2K1left,ts2k1right),rest671) end
| (677,(_,(MlyValue.from_environment from_environment,_,
from_environmentright as from_environment1right))::(_,(
MlyValue.identifier identifier,_,_))::(_,(_,ACCEPTleft as ACCEPT1left,
_))::rest671) => let val result=MlyValue.accept_statement((
Cobol.ACCEPT_ENVIRONMENT(identifier, from_environment,ACCEPTleft,from_environmentright)
))
 in (hojfelds_NT 7,(result,ACCEPT1left,from_environment1right),rest671)
 end
| (678,(_,(MlyValue.date_day_time date_day_time,_,date_day_timeright
 as date_day_time1right))::_::(_,(MlyValue.identifier identifier,_,_))
::(_,(_,ACCEPTleft as ACCEPT1left,_))::rest671) => let val result=
MlyValue.accept_statement((
Cobol.ACCEPT_DATE_DAY_TIME(identifier, date_day_time,ACCEPTleft,date_day_timeright)
))
 in (hojfelds_NT 7,(result,ACCEPT1left,date_day_time1right),rest671)
 end
| (679,(_,(MlyValue.mnemonic_name mnemonic_name,_,mnemonic_nameright
 as mnemonic_name1right))::(_,(_,FROMleft as FROM1left,_))::rest671)
 => let val result=MlyValue.from_environment((
Cobol.FROM(mnemonic_name,FROMleft,mnemonic_nameright)))
 in (hojfelds_NT 215,(result,FROM1left,mnemonic_name1right),rest671)
 end
| (680,rest671) => let val result=MlyValue.from_environment((
Cobol.NO_FROM))
 in (hojfelds_NT 215,(result,defaultPos,defaultPos),rest671) end
| (681,(_,(_,DATEleft as DATE1left,DATEright as DATE1right))::rest671)
 => let val result=MlyValue.date_day_time((
Cobol.DATE(DATEleft,DATEright)))
 in (hojfelds_NT 112,(result,DATE1left,DATE1right),rest671) end
| (682,(_,(_,DAYleft as DAY1left,DAYright as DAY1right))::rest671) => 
let val result=MlyValue.date_day_time((Cobol.DAY(DAYleft,DAYright)))
 in (hojfelds_NT 112,(result,DAY1left,DAY1right),rest671) end
| (683,(_,(_,TIMEleft as TIME1left,TIMEright as TIME1right))::rest671)
 => let val result=MlyValue.date_day_time((
Cobol.TIME(TIMEleft,TIMEright)))
 in (hojfelds_NT 112,(result,TIME1left,TIME1right),rest671) end
| (684,(_,(_,_,end_addright as end_add1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.identifier_roundeds identifier_roundeds,_,_))::_::(_,(
MlyValue.identifier_or_literals identifier_or_literals,_,_))::(_,(_,
ADDleft as ADD1left,_))::rest671) => let val result=
MlyValue.add_statement((
Cobol.ADD_OR_SUBTRACT
                    (Cobol.ADD,identifier_or_literals,identifier_roundeds,
                     size_error_clauses,
                     ADDleft,end_addright)
))
 in (hojfelds_NT 10,(result,ADD1left,end_add1right),rest671) end
| (685,(_,(_,_,end_addright as end_add1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.identifier_roundeds identifier_roundeds,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal,_,_))::(_,(
MlyValue.identifier_or_literals identifier_or_literals,_,_))::(_,(_,
ADDleft as ADD1left,_))::rest671) => let val result=
MlyValue.add_statement((
Cobol.ADD_OR_SUBTRACT_GIVING
           (Cobol.ADD,identifier_or_literals,identifier_or_literal,
            identifier_roundeds,
            size_error_clauses,ADDleft,end_addright)
))
 in (hojfelds_NT 10,(result,ADD1left,end_add1right),rest671) end
| (686,(_,(_,_,end_addright as end_add1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.rounded rounded,_,_))::(_,(MlyValue.identifier identifier2,_,
_))::_::(_,(MlyValue.identifier identifier1,_,_))::_::(_,(_,ADDleft
 as ADD1left,_))::rest671) => let val result=MlyValue.add_statement((
Cobol.ADD_OR_SUBTRACT_CORRESPONDING
                (Cobol.ADD,identifier1,identifier2,rounded,
                 size_error_clauses,ADDleft,end_addright)
))
 in (hojfelds_NT 10,(result,ADD1left,end_add1right),rest671) end
| (687,(_,(MlyValue.identifier_rounded identifier_rounded,_,
identifier_rounded1right))::(_,(MlyValue.identifier_roundeds 
identifier_roundeds,identifier_roundeds1left,_))::rest671) => let val 
result=MlyValue.identifier_roundeds((
Cobol.SEVERAL_IDENTIFIER_ROUNDEDS(identifier_roundeds,
                                             identifier_rounded)
))
 in (hojfelds_NT 236,(result,identifier_roundeds1left,
identifier_rounded1right),rest671) end
| (688,(_,(MlyValue.identifier_rounded identifier_rounded,
identifier_rounded1left,identifier_rounded1right))::rest671) => let 
val result=MlyValue.identifier_roundeds((
Cobol.ONE_IDENTIFIER_ROUNDED(identifier_rounded)))
 in (hojfelds_NT 236,(result,identifier_rounded1left,
identifier_rounded1right),rest671) end
| (689,(_,(MlyValue.rounded rounded,_,rounded1right))::(_,(
MlyValue.identifier identifier,identifier1left,_))::rest671) => let 
val result=MlyValue.identifier_rounded((
Cobol.IDENTIFIER_ROUNDED(identifier,rounded)))
 in (hojfelds_NT 235,(result,identifier1left,rounded1right),rest671)
 end
| (690,(_,(MlyValue.size_error_clause size_error_clause,
size_error_clause1left,size_error_clause1right))::rest671) => let val 
result=MlyValue.size_error_clauses((size_error_clause))
 in (hojfelds_NT 442,(result,size_error_clause1left,
size_error_clause1right),rest671) end
| (691,(_,(MlyValue.imperative_statement imperative_statement,_,
imperative_statement1right))::_::_::(_,(_,on1left,_))::rest671) => 
let val result=MlyValue.size_error_clause((
Cobol.SIZE_ERROR(imperative_statement)))
 in (hojfelds_NT 441,(result,on1left,imperative_statement1right),
rest671) end
| (692,rest671) => let val result=MlyValue.size_error_clause((
Cobol.NO_SIZE_ERROR))
 in (hojfelds_NT 441,(result,defaultPos,defaultPos),rest671) end
| (693,(_,(_,_,alterations1right))::(_,(_,ALTER1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 32,(result,ALTER1left,alterations1right),rest671) end
| (694,(_,(_,_,alteration1right))::(_,(_,alterations1left,_))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 34,(result,alterations1left,alteration1right),rest671)
 end
| (695,(_,(_,alteration1left,alteration1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 34,(result,alteration1left,alteration1right),rest671)
 end
| (696,(_,(_,_,procedure_name2right))::_::_::(_,(_,procedure_name1left
,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 33,(result,procedure_name1left,procedure_name2right),
rest671) end
| (697,(_,(_,_,end_call1right))::_::_::_::(_,(_,CALL1left,_))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 55,(result,CALL1left,end_call1right),rest671) end
| (698,(_,(_,_,identifiers1right))::(_,(_,USING1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 513,(result,USING1left,identifiers1right),rest671) end
| (699,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 513,(result,defaultPos,defaultPos),rest671) end
| (700,(_,(_,_,identifier_or_literals1right))::(_,(_,CANCEL1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 56,(result,CANCEL1left,identifier_or_literals1right),
rest671) end
| (701,(_,(_,_,file_name_specifications1right))::(_,(_,CLOSE1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 63,(result,CLOSE1left,file_name_specifications1right),
rest671) end
| (702,(_,(_,_,file_name_specification1right))::(_,(_,
file_name_specifications1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 202,(result,file_name_specifications1left,
file_name_specification1right),rest671) end
| (703,(_,(_,file_name_specification1left,
file_name_specification1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 202,(result,file_name_specification1left,
file_name_specification1right),rest671) end
| (704,(_,(_,_,closing_specification1right))::(_,(_,file_name1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 201,(result,file_name1left,closing_specification1right
),rest671) end
| (705,(_,(_,_,for_removal1right))::(_,(_,reel_or_unit1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 64,(result,reel_or_unit1left,for_removal1right),
rest671) end
| (706,(_,(_,_,no_rewind_or_lock1right))::(_,(_,with1left,_))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 64,(result,with1left,no_rewind_or_lock1right),rest671)
 end
| (707,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 64,(result,defaultPos,defaultPos),rest671) end
| (708,(_,(_,REEL1left,REEL1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 392,(result,REEL1left,REEL1right),rest671) end
| (709,(_,(_,UNIT1left,UNIT1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 392,(result,UNIT1left,UNIT1right),rest671) end
| (710,(_,(_,_,REWIND1right))::(_,(_,NO1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 320,(result,NO1left,REWIND1right),rest671) end
| (711,(_,(_,LOCK1left,LOCK1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 320,(result,LOCK1left,LOCK1right),rest671) end
| (712,(_,(_,_,end_computeright as end_compute1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.arithmetic_expression arithmetic_expression,_,_))::_::(_,(
MlyValue.identifier_roundeds identifier_roundeds,_,_))::(_,(_,
COMPUTEleft as COMPUTE1left,_))::rest671) => let val result=
MlyValue.compute_statement((
Cobol.COMPUTE(identifier_roundeds,
                                   arithmetic_expression,
                                   size_error_clauses,
                                   COMPUTEleft,end_computeright)
))
 in (hojfelds_NT 72,(result,COMPUTE1left,end_compute1right),rest671)
 end
| (713,(_,(_,_,end_delete1right))::_::_::_::(_,(_,DELETE1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 120,(result,DELETE1left,end_delete1right),rest671) end
| (714,(_,(_,_,not_invalid_key_error_clause1right))::(_,(_,
invalid_key_error_clause1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 272,(result,invalid_key_error_clause1left,
not_invalid_key_error_clause1right),rest671) end
| (715,(_,(_,_,imperative_statement1right))::_::(_,(_,INVALID1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 270,(result,INVALID1left,imperative_statement1right),
rest671) end
| (716,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 270,(result,defaultPos,defaultPos),rest671) end
| (717,(_,(_,_,imperative_statement1right))::_::_::(_,(_,NOT1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 324,(result,NOT1left,imperative_statement1right),
rest671) end
| (718,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 324,(result,defaultPos,defaultPos),rest671) end
| (719,(_,(_,_,upon_mnemonic_name1right))::_::(_,(_,DISPLAY1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 126,(result,DISPLAY1left,upon_mnemonic_name1right),
rest671) end
| (720,(_,(_,_,mnemonic_name1right))::(_,(_,UPON1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 506,(result,UPON1left,mnemonic_name1right),rest671)
 end
| (721,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 506,(result,defaultPos,defaultPos),rest671) end
| (722,(_,(_,_,end_divideright as end_divide1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.identifier_roundeds identifier_roundeds,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal,_,_))::(_,(_,
DIVIDEleft as DIVIDE1left,_))::rest671) => let val result=
MlyValue.divide_statement((
Cobol.DIVIDE_INTO(identifier_or_literal,identifier_roundeds,
                             size_error_clauses,DIVIDEleft,end_divideright)
))
 in (hojfelds_NT 127,(result,DIVIDE1left,end_divide1right),rest671) end
| (723,(_,(_,_,end_divideright as end_divide1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.identifier_roundeds identifier_roundeds,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal2,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal1,_,_))::(_,(_,
DIVIDEleft as DIVIDE1left,_))::rest671) => let val result=
MlyValue.divide_statement((
Cobol.DIVIDE_INTO_GIVING
                            (identifier_or_literal1,identifier_or_literal2,
                             identifier_roundeds, size_error_clauses,
                             DIVIDEleft,end_divideright)
))
 in (hojfelds_NT 127,(result,DIVIDE1left,end_divide1right),rest671) end
| (724,(_,(_,_,end_divideright as end_divide1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.identifier_roundeds identifier_roundeds,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal2,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal1,_,_))::(_,(_,
DIVIDEleft as DIVIDE1left,_))::rest671) => let val result=
MlyValue.divide_statement((
Cobol.DIVIDE_BY_GIVING
                            (identifier_or_literal1,identifier_or_literal2,
                             identifier_roundeds,size_error_clauses,
                             DIVIDEleft,end_divideright)
))
 in (hojfelds_NT 127,(result,DIVIDE1left,end_divide1right),rest671) end
| (725,(_,(_,_,end_divideright as end_divide1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.identifier identifier,_,_))::_::(_,(
MlyValue.identifier_roundeds identifier_roundeds,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal2,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal1,_,_))::(_,(_,
DIVIDEleft as DIVIDE1left,_))::rest671) => let val result=
MlyValue.divide_statement((
Cobol.DIVIDE_INTO_GIVING_REMAINDER
                            (identifier_or_literal1,identifier_or_literal2,
                             identifier_roundeds,identifier,size_error_clauses,
                             DIVIDEleft,end_divideright)
))
 in (hojfelds_NT 127,(result,DIVIDE1left,end_divide1right),rest671) end
| (726,(_,(_,_,end_divideright as end_divide1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.identifier identifier,_,_))::_::(_,(
MlyValue.identifier_roundeds identifier_roundeds,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal2,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal1,_,_))::(_,(_,
DIVIDEleft as DIVIDE1left,_))::rest671) => let val result=
MlyValue.divide_statement((
Cobol.DIVIDE_BY_GIVING_REMAINDER
                            (identifier_or_literal1,identifier_or_literal2,
                             identifier_roundeds,identifier,size_error_clauses,
                             DIVIDEleft,end_divideright)
))
 in (hojfelds_NT 127,(result,DIVIDE1left,end_divide1right),rest671) end
| (727,(_,(_,_,routine_name_opt1right))::_::(_,(_,ENTER1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 151,(result,ENTER1left,routine_name_opt1right),rest671
) end
| (728,(_,(_,_,identifiers1right))::_::(_,(_,EXHIBIT1left,_))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 161,(result,EXHIBIT1left,identifiers1right),rest671)
 end
| (729,(_,(_,NAMED1left,NAMED1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 58,(result,NAMED1left,NAMED1right),rest671) end
| (730,(_,(_,_,NAMED1right))::(_,(_,CHANGED1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 58,(result,CHANGED1left,NAMED1right),rest671) end
| (731,(_,(_,EXIT1left,EXIT1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 163,(result,EXIT1left,EXIT1right),rest671) end
| (732,(_,(_,_,PROGRAM1right))::(_,(_,EXIT1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 162,(result,EXIT1left,PROGRAM1right),rest671) end
| (733,(_,(_,_,procedure_name_opt1right))::_::(_,(_,GO1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 218,(result,GO1left,procedure_name_opt1right),rest671)
 end
| (734,(_,(_,_,identifier1right))::_::_::_::_::(_,(_,GO1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 218,(result,GO1left,identifier1right),rest671) end
| (735,(_,(MlyValue.else_statements else_statements,_,
else_statements1right))::(_,(MlyValue.then_statements then_statements,
_,_))::(_,(_,_,thenright))::(_,(MlyValue.conditional_expression 
conditional_expression,_,_))::(_,(_,IFleft as IF1left,_))::rest671)
 => let val result=MlyValue.if_statement((
Cobol.IF(conditional_expression,then_statements,else_statements,
                    IFleft,thenright)
))
 in (hojfelds_NT 240,(result,IF1left,else_statements1right),rest671)
 end
| (736,(_,(MlyValue.imperative_statement imperative_statement,
imperative_statement1left,imperative_statement1right))::rest671) => 
let val result=MlyValue.then_statements((
Cobol.THEN_STATEMENTS(imperative_statement)))
 in (hojfelds_NT 490,(result,imperative_statement1left,
imperative_statement1right),rest671) end
| (737,(_,(_,_,SENTENCE1right))::(_,(_,NEXT1left,_))::rest671) => let 
val result=MlyValue.then_statements((Cobol.NEXT_SENTENCE))
 in (hojfelds_NT 490,(result,NEXT1left,SENTENCE1right),rest671) end
| (738,(_,(_,_,end_if1right))::(_,(MlyValue.statements statements,_,_)
)::(_,(_,ELSE1left,_))::rest671) => let val result=
MlyValue.else_statements((Cobol.ELSE(statements)))
 in (hojfelds_NT 130,(result,ELSE1left,end_if1right),rest671) end
| (739,(_,(_,_,SENTENCE1right))::_::(_,(_,ELSE1left,_))::rest671) => 
let val result=MlyValue.else_statements((Cobol.ELSE_NEXT_SENTENCE))
 in (hojfelds_NT 130,(result,ELSE1left,SENTENCE1right),rest671) end
| (740,(_,(_,end_if1left,end_if1right))::rest671) => let val result=
MlyValue.else_statements((Cobol.NO_ELSE))
 in (hojfelds_NT 130,(result,end_if1left,end_if1right),rest671) end
| (741,(_,(_,_,identifier_tallying_specifications1right))::_::_::(_,(_
,INSPECT1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 262,(result,INSPECT1left,
identifier_tallying_specifications1right),rest671) end
| (742,(_,(_,_,inspect_replacing_specifications1right))::_::_::(_,(_,
INSPECT1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 262,(result,INSPECT1left,
inspect_replacing_specifications1right),rest671) end
| (743,(_,(_,_,inspect_replacing_specifications1right))::_::_::_::_::(
_,(_,INSPECT1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 262,(result,INSPECT1left,
inspect_replacing_specifications1right),rest671) end
| (744,(_,(_,_,tallying_specifications1right))::_::(_,(_,
identifier1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 238,(result,identifier1left,
tallying_specifications1right),rest671) end
| (745,(_,(_,_,before_after_phrase1right))::(_,(_,CHARACTERS1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 482,(result,CHARACTERS1left,before_after_phrase1right)
,rest671) end
| (746,(_,(_,_,all_or_leading_specifications1right))::(_,(_,
all_or_leading1left,_))::rest671) => let val result=MlyValue.ntVOID(()
)
 in (hojfelds_NT 482,(result,all_or_leading1left,
all_or_leading_specifications1right),rest671) end
| (747,(_,(_,_,tallying_specifications1right))::_::(_,(_,
CHARACTERS1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 482,(result,CHARACTERS1left,
tallying_specifications1right),rest671) end
| (748,(_,(_,_,tallying_specifications1right))::_::(_,(_,
all_or_leading1left,_))::rest671) => let val result=MlyValue.ntVOID(()
)
 in (hojfelds_NT 482,(result,all_or_leading1left,
tallying_specifications1right),rest671) end
| (749,(_,(_,_,identifier_tallying_specifications1right))::_::(_,(_,
CHARACTERS1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 482,(result,CHARACTERS1left,
identifier_tallying_specifications1right),rest671) end
| (750,(_,(_,_,identifier_tallying_specifications1right))::_::(_,(_,
all_or_leading1left,_))::rest671) => let val result=MlyValue.ntVOID(()
)
 in (hojfelds_NT 482,(result,all_or_leading1left,
identifier_tallying_specifications1right),rest671) end
| (751,(_,(_,_,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1right)
)::_::(_,(_,before_after1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 168,(result,before_after1left,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1right)
,rest671) end
| (752,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 168,(result,defaultPos,defaultPos),rest671) end
| (753,(_,(_,BEFORE1left,BEFORE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 51,(result,BEFORE1left,BEFORE1right),rest671) end
| (754,(_,(_,AFTER1left,AFTER1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 51,(result,AFTER1left,AFTER1right),rest671) end
| (755,(_,(_,ALL1left,ALL1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 19,(result,ALL1left,ALL1right),rest671) end
| (756,(_,(_,LEADING1left,LEADING1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 19,(result,LEADING1left,LEADING1right),rest671) end
| (757,(_,(_,_,all_or_leading_specification1right))::(_,(_,
all_or_leading_specifications1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 22,(result,all_or_leading_specifications1left,
all_or_leading_specification1right),rest671) end
| (758,(_,(_,all_or_leading_specification1left,
all_or_leading_specification1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 22,(result,all_or_leading_specification1left,
all_or_leading_specification1right),rest671) end
| (759,(_,(_,_,before_after_phrase1right))::(_,(_,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1left,_
))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 21,(result,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1left,
before_after_phrase1right),rest671) end
| (760,(_,(_,_,inspect_replacing_specification1right))::(_,(_,
inspect_replacing_specifications1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 167,(result,inspect_replacing_specifications1left,
inspect_replacing_specification1right),rest671) end
| (761,(_,(_,inspect_replacing_specification1left,
inspect_replacing_specification1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 167,(result,inspect_replacing_specification1left,
inspect_replacing_specification1right),rest671) end
| (762,(_,(_,_,before_after_phrase1right))::_::_::(_,(_,
CHARACTERS1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 169,(result,CHARACTERS1left,before_after_phrase1right)
,rest671) end
| (763,(_,(_,_,all_leading_first_specifications1right))::(_,(_,
all_or_leading_or_first1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 169,(result,all_or_leading_or_first1left,
all_leading_first_specifications1right),rest671) end
| (764,(_,(_,ALL1left,ALL1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 20,(result,ALL1left,ALL1right),rest671) end
| (765,(_,(_,LEADING1left,LEADING1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 20,(result,LEADING1left,LEADING1right),rest671) end
| (766,(_,(_,FIRST1left,FIRST1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 20,(result,FIRST1left,FIRST1right),rest671) end
| (767,(_,(_,_,all_leading_first_specification1right))::(_,(_,
all_leading_first_specifications1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 17,(result,all_leading_first_specifications1left,
all_leading_first_specification1right),rest671) end
| (768,(_,(_,all_leading_first_specification1left,
all_leading_first_specification1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 17,(result,all_leading_first_specification1left,
all_leading_first_specification1right),rest671) end
| (769,(_,(_,_,before_after_phrase1right))::_::_::(_,(_,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1left,_
))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 16,(result,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1left,
before_after_phrase1right),rest671) end
| (770,(_,(_,_,output_procedure_or_giving_phrase1right))::_::_::_::_::
(_,(_,MERGE1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 308,(result,MERGE1left,
output_procedure_or_giving_phrase1right),rest671) end
| (771,(_,(_,_,on_ascending_descending_key_phrase1right))::(_,(_,
on_ascending_descending_key_phrases1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 337,(result,on_ascending_descending_key_phrases1left,
on_ascending_descending_key_phrase1right),rest671) end
| (772,(_,(_,on_ascending_descending_key_phrase1left,
on_ascending_descending_key_phrase1right))::rest671) => let val result
=MlyValue.ntVOID(())
 in (hojfelds_NT 337,(result,on_ascending_descending_key_phrase1left,
on_ascending_descending_key_phrase1right),rest671) end
| (773,(_,(_,_,data_names1right))::_::_::(_,(_,on1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 336,(result,on1left,data_names1right),rest671) end
| (774,(_,(_,_,alphabet_name1right))::_::_::(_,(_,collating1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 68,(result,collating1left,alphabet_name1right),rest671
) end
| (775,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 68,(result,defaultPos,defaultPos),rest671) end
| (776,(_,(_,_,file_names1right))::_::(_,(_,USING1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 515,(result,USING1left,file_names1right),rest671) end
| (777,(_,(_,output_procedure_phrase1left,
output_procedure_phrase1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 353,(result,output_procedure_phrase1left,
output_procedure_phrase1right),rest671) end
| (778,(_,(_,giving_phrase1left,giving_phrase1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 353,(result,giving_phrase1left,giving_phrase1right),
rest671) end
| (779,(_,(_,_,procedure_range1right))::_::_::(_,(_,OUTPUT1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 354,(result,OUTPUT1left,procedure_range1right),rest671
) end
| (780,(_,(_,procedure_name1left,procedure_name1right))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 170,(result,procedure_name1left,procedure_name1right),
rest671) end
| (781,(_,(_,_,procedure_name2right))::_::(_,(_,procedure_name1left,_)
)::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 170,(result,procedure_name1left,procedure_name2right),
rest671) end
| (782,(_,(_,_,file_names1right))::(_,(_,GIVING1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 217,(result,GIVING1left,file_names1right),rest671) end
| (783,(_,(MlyValue.identifiers identifiers,_,identifiersright as 
identifiers1right))::_::(_,(MlyValue.identifier_or_literal 
identifier_or_literal,_,_))::(_,(_,MOVEleft as MOVE1left,_))::rest671)
 => let val result=MlyValue.move_statement((
Cobol.MOVE(identifier_or_literal,identifiers,
                      MOVEleft,identifiersright)
))
 in (hojfelds_NT 314,(result,MOVE1left,identifiers1right),rest671) end
| (784,(_,(MlyValue.identifier identifier2,_,identifier2right))::_::(_
,(MlyValue.identifier identifier1,_,identifierright))::_::(_,(_,
MOVEleft as MOVE1left,_))::rest671) => let val result=
MlyValue.move_statement((
Cobol.MOVECORRESPONDING(identifier1,identifier2,
                      MOVEleft,identifierright)
))
 in (hojfelds_NT 314,(result,MOVE1left,identifier2right),rest671) end
| (785,(_,(_,_,end_multiplyright as end_multiply1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.identifier_roundeds identifier_roundeds,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal,_,_))::(_,(_,
MULTIPLYleft as MULTIPLY1left,_))::rest671) => let val result=
MlyValue.multiply_statement((
Cobol.MULTIPLY(identifier_or_literal,identifier_roundeds
                          ,size_error_clauses,MULTIPLYleft,end_multiplyright)
))
 in (hojfelds_NT 317,(result,MULTIPLY1left,end_multiply1right),rest671)
 end
| (786,(_,(_,_,end_multiplyright as end_multiply1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.identifier_roundeds identifier_roundeds,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal2,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal1,_,_))::(_,(_,
MULTIPLYleft as MULTIPLY1left,_))::rest671) => let val result=
MlyValue.multiply_statement((
Cobol.MULTIPLY_GIVING(identifier_or_literal1,identifier_or_literal2
                                 ,identifier_roundeds,size_error_clauses
                                 ,MULTIPLYleft,end_multiplyright)
))
 in (hojfelds_NT 317,(result,MULTIPLY1left,end_multiply1right),rest671)
 end
| (787,(_,(_,_,io_phrases1right))::(_,(_,OPEN1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 341,(result,OPEN1left,io_phrases1right),rest671) end
| (788,(_,(_,_,io_phrase1right))::(_,(_,io_phrases1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 274,(result,io_phrases1left,io_phrase1right),rest671)
 end
| (789,(_,(_,io_phrase1left,io_phrase1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 274,(result,io_phrase1left,io_phrase1right),rest671)
 end
| (790,(_,(_,_,file_input_specifications1right))::(_,(_,INPUT1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 273,(result,INPUT1left,file_input_specifications1right
),rest671) end
| (791,(_,(_,_,file_output_specifications1right))::(_,(_,OUTPUT1left,_
))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 273,(result,OUTPUT1left,
file_output_specifications1right),rest671) end
| (792,(_,(_,_,file_names1right))::(_,(_,IO1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 273,(result,IO1left,file_names1right),rest671) end
| (793,(_,(_,_,file_names1right))::(_,(_,EXTEND1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 273,(result,EXTEND1left,file_names1right),rest671) end
| (794,(_,(_,_,file_input_specification1right))::(_,(_,
file_input_specifications1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 199,(result,file_input_specifications1left,
file_input_specification1right),rest671) end
| (795,(_,(_,file_input_specification1left,
file_input_specification1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 199,(result,file_input_specification1left,
file_input_specification1right),rest671) end
| (796,(_,(_,_,file_input_specification_attribute1right))::(_,(_,
file_name1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 197,(result,file_name1left,
file_input_specification_attribute1right),rest671) end
| (797,(_,(_,REVERSED1left,REVERSED1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 198,(result,REVERSED1left,REVERSED1right),rest671) end
| (798,(_,(_,_,REWIND1right))::_::(_,(_,with1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 198,(result,with1left,REWIND1right),rest671) end
| (799,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 198,(result,defaultPos,defaultPos),rest671) end
| (800,(_,(_,_,file_output_specification1right))::(_,(_,
file_output_specifications1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 206,(result,file_output_specifications1left,
file_output_specification1right),rest671) end
| (801,(_,(_,file_output_specification1left,
file_output_specification1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 206,(result,file_output_specification1left,
file_output_specification1right),rest671) end
| (802,(_,(_,_,file_output_specification_attribute1right))::(_,(_,
file_name1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 204,(result,file_name1left,
file_output_specification_attribute1right),rest671) end
| (803,(_,(_,_,REWIND1right))::_::(_,(_,with1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 205,(result,with1left,REWIND1right),rest671) end
| (804,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 205,(result,defaultPos,defaultPos),rest671) end
| (805,(_,(_,_,procedure_range1right))::(_,(_,PERFORM1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 360,(result,PERFORM1left,procedure_range1right),
rest671) end
| (806,(_,(_,_,times_phrase1right))::_::(_,(_,PERFORM1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 360,(result,PERFORM1left,times_phrase1right),rest671)
 end
| (807,(_,(_,_,until_phrase1right))::_::(_,(_,PERFORM1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 360,(result,PERFORM1left,until_phrase1right),rest671)
 end
| (808,(_,(_,_,test_varying_phrase1right))::_::(_,(_,PERFORM1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 360,(result,PERFORM1left,test_varying_phrase1right),
rest671) end
| (809,(_,(_,_,TIMES1right))::(_,(_,identifier_or_integer1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 498,(result,identifier_or_integer1left,TIMES1right),
rest671) end
| (810,(_,(_,_,conditional_expression1right))::(_,(_,UNTIL1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 484,(result,UNTIL1left,conditional_expression1right),
rest671) end
| (811,(_,(_,_,after_phrases1right))::_::_::_::_::_::_::_::(_,(_,
VARYING1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 485,(result,VARYING1left,after_phrases1right),rest671)
 end
| (812,(_,(_,_,after_phrase1right))::(_,(_,after_phrases1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 14,(result,after_phrases1left,after_phrase1right),
rest671) end
| (813,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 14,(result,defaultPos,defaultPos),rest671) end
| (814,(_,(_,_,conditional_expression1right))::_::_::_::_::_::_::(_,(_
,AFTER1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 13,(result,AFTER1left,conditional_expression1right),
rest671) end
| (815,(_,(_,_,end_read1right))::_::_::_::_::_::_::(_,(_,READ1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 377,(result,READ1left,end_read1right),rest671) end
| (816,(_,(_,_,identifier1right))::(_,(_,INTO1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 266,(result,INTO1left,identifier1right),rest671) end
| (817,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 266,(result,defaultPos,defaultPos),rest671) end
| (818,(_,(_,_,data_name1right))::_::(_,(_,KEY1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 283,(result,KEY1left,data_name1right),rest671) end
| (819,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 283,(result,defaultPos,defaultPos),rest671) end
| (820,(_,(_,end_error_clauses1left,end_error_clauses1right))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 141,(result,end_error_clauses1left,
end_error_clauses1right),rest671) end
| (821,(_,(_,invalid_key_error_clauses1left,
invalid_key_error_clauses1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 141,(result,invalid_key_error_clauses1left,
invalid_key_error_clauses1right),rest671) end
| (822,(_,(_,_,not_end_error_clause1right))::(_,(_,
end_error_clause1left,_))::rest671) => let val result=MlyValue.ntVOID(
())
 in (hojfelds_NT 137,(result,end_error_clause1left,
not_end_error_clause1right),rest671) end
| (823,(_,(_,_,imperative_statement1right))::_::(_,(_,at1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 136,(result,at1left,imperative_statement1right),
rest671) end
| (824,(_,(_,_,imperative_statement1right))::_::_::(_,(_,NOT1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 321,(result,NOT1left,imperative_statement1right),
rest671) end
| (825,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 321,(result,defaultPos,defaultPos),rest671) end
| (826,(_,(_,_,from_identifier1right))::_::(_,(_,RELEASE1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 400,(result,RELEASE1left,from_identifier1right),
rest671) end
| (827,(_,(_,_,identifier1right))::(_,(_,FROM1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 216,(result,FROM1left,identifier1right),rest671) end
| (828,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 216,(result,defaultPos,defaultPos),rest671) end
| (829,(_,(_,_,end_return1right))::_::_::_::_::(_,(_,RETURN1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 411,(result,RETURN1left,end_return1right),rest671) end
| (830,(_,(_,_,end_rewrite1right))::_::_::_::(_,(_,REWRITE1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 412,(result,REWRITE1left,end_rewrite1right),rest671)
 end
| (831,(_,(_,_,end_search1right))::_::_::_::_::(_,(_,SEARCH1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 421,(result,SEARCH1left,end_search1right),rest671) end
| (832,(_,(_,_,end_search1right))::_::_::_::_::_::_::(_,(_,SEARCH1left
,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 421,(result,SEARCH1left,end_search1right),rest671) end
| (833,(_,(_,_,identifier1right))::(_,(_,VARYING1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 523,(result,VARYING1left,identifier1right),rest671)
 end
| (834,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 523,(result,defaultPos,defaultPos),rest671) end
| (835,(_,(_,end_error_clause1left,end_error_clause1right))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 173,(result,end_error_clause1left,
end_error_clause1right),rest671) end
| (836,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 173,(result,defaultPos,defaultPos),rest671) end
| (837,(_,(_,_,search_when_phrase1right))::(_,(_,
search_when_phrases1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 172,(result,search_when_phrases1left,
search_when_phrase1right),rest671) end
| (838,(_,(_,search_when_phrase1left,search_when_phrase1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 172,(result,search_when_phrase1left,
search_when_phrase1right),rest671) end
| (839,(_,(_,_,then_statement1right))::_::(_,(_,WHEN1left,_))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 174,(result,WHEN1left,then_statement1right),rest671)
 end
| (840,(_,(_,_,identifier_or_integer1right))::_::_::(_,(_,SET1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 435,(result,SET1left,identifier_or_integer1right),
rest671) end
| (841,(_,(_,_,identifier_or_integer1right))::_::_::(_,(_,SET1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 435,(result,SET1left,identifier_or_integer1right),
rest671) end
| (842,(_,(_,_,mnemonic_switches1right))::(_,(_,SET1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 435,(result,SET1left,mnemonic_switches1right),rest671)
 end
| (843,(_,(_,_,TRUE1right))::_::_::(_,(_,SET1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 435,(result,SET1left,TRUE1right),rest671) end
| (844,(_,(_,_,BY1right))::(_,(_,UP1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 505,(result,UP1left,BY1right),rest671) end
| (845,(_,(_,_,BY1right))::(_,(_,DOWN1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 505,(result,DOWN1left,BY1right),rest671) end
| (846,(_,(_,_,mnemonic_switch1right))::(_,(_,mnemonic_switches1left,_
))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 312,(result,mnemonic_switches1left,
mnemonic_switch1right),rest671) end
| (847,(_,(_,mnemonic_switch1left,mnemonic_switch1right))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 312,(result,mnemonic_switch1left,mnemonic_switch1right
),rest671) end
| (848,(_,(_,_,on_off1right))::_::(_,(_,identifiers1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 311,(result,identifiers1left,on_off1right),rest671)
 end
| (849,(_,(_,ON1left,ON1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 338,(result,ON1left,ON1right),rest671) end
| (850,(_,(_,OFF1left,OFF1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 338,(result,OFF1left,OFF1right),rest671) end
| (851,(_,(_,_,output_procedure_or_giving_phrase1right))::_::_::_::_::
(_,(_,SORT1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 448,(result,SORT1left,
output_procedure_or_giving_phrase1right),rest671) end
| (852,(_,(_,input_procedure_phrase1left,input_procedure_phrase1right)
)::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 176,(result,input_procedure_phrase1left,
input_procedure_phrase1right),rest671) end
| (853,(_,(_,sort_using_phrase1left,sort_using_phrase1right))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 176,(result,sort_using_phrase1left,
sort_using_phrase1right),rest671) end
| (854,(_,(_,_,procedure_range1right))::_::_::(_,(_,INPUT1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 261,(result,INPUT1left,procedure_range1right),rest671)
 end
| (855,(_,(_,_,file_names1right))::(_,(_,USING1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 175,(result,USING1left,file_names1right),rest671) end
| (856,(_,(_,_,end_start1right))::_::_::_::(_,(_,START1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 457,(result,START1left,end_start1right),rest671) end
| (857,(_,(_,_,qdata_names1right))::_::_::(_,(_,KEY1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 284,(result,KEY1left,qdata_names1right),rest671) end
| (858,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 284,(result,defaultPos,defaultPos),rest671) end
| (859,(_,(_,_,RUN1right))::(_,(_,STOP1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 466,(result,STOP1left,RUN1right),rest671) end
| (860,(_,(_,_,literal1right))::(_,(_,STOP1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 466,(result,STOP1left,literal1right),rest671) end
| (861,(_,(_,_,end_string1right))::_::_::_::_::_::(_,(_,STRING1left,_)
)::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 469,(result,STRING1left,end_string1right),rest671) end
| (862,(_,(_,_,string_specification1right))::(_,(_,
string_specifications1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 468,(result,string_specifications1left,
string_specification1right),rest671) end
| (863,(_,(_,string_specification1left,string_specification1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 468,(result,string_specification1left,
string_specification1right),rest671) end
| (864,(_,(_,_,identifier_literal_size1right))::_::_::(_,(_,
identifier_or_nonnumeric_literal_or_not_all_figurative_constants1left,
_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 467,(result,
identifier_or_nonnumeric_literal_or_not_all_figurative_constants1left,
identifier_literal_size1right),rest671) end
| (865,(_,(_,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1left,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1right)
)::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 231,(result,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1left,
identifier_or_nonnumeric_literal_or_not_all_figurative_constant1right)
,rest671) end
| (866,(_,(_,SIZE1left,SIZE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 231,(result,SIZE1left,SIZE1right),rest671) end
| (867,(_,(_,_,identifier1right))::_::(_,(_,with1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 364,(result,with1left,identifier1right),rest671) end
| (868,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 364,(result,defaultPos,defaultPos),rest671) end
| (869,(_,(_,_,imperative_statement1right))::_::(_,(_,on1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 355,(result,on1left,imperative_statement1right),
rest671) end
| (870,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 355,(result,defaultPos,defaultPos),rest671) end
| (871,(_,(_,_,end_subtractright as end_subtract1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.identifier_roundeds identifier_roundeds,_,_))::_::(_,(
MlyValue.identifier_or_literals identifier_or_literals,_,_))::(_,(_,
SUBTRACTleft as SUBTRACT1left,_))::rest671) => let val result=
MlyValue.subtract_statement((
Cobol.ADD_OR_SUBTRACT
                    (Cobol.SUBTRACT,identifier_or_literals,identifier_roundeds,
                     size_error_clauses,
                     SUBTRACTleft,end_subtractright)
))
 in (hojfelds_NT 473,(result,SUBTRACT1left,end_subtract1right),rest671)
 end
| (872,(_,(_,_,end_subtractright as end_subtract1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.identifier_roundeds identifier_roundeds,_,_))::_::(_,(
MlyValue.identifier_or_literal identifier_or_literal,_,_))::_::(_,(
MlyValue.identifier_or_literals identifier_or_literals,_,_))::(_,(_,
SUBTRACTleft as SUBTRACT1left,_))::rest671) => let val result=
MlyValue.subtract_statement((
Cobol.ADD_OR_SUBTRACT_GIVING
           (Cobol.SUBTRACT,identifier_or_literals,identifier_or_literal,
            identifier_roundeds,
            size_error_clauses,SUBTRACTleft,end_subtractright)
))
 in (hojfelds_NT 473,(result,SUBTRACT1left,end_subtract1right),rest671)
 end
| (873,(_,(_,_,end_subtractright as end_subtract1right))::(_,(
MlyValue.size_error_clauses size_error_clauses,_,_))::(_,(
MlyValue.rounded rounded,_,_))::(_,(MlyValue.identifier identifier2,_,
_))::_::(_,(MlyValue.identifier identifier1,_,_))::_::(_,(_,
SUBTRACTleft as SUBTRACT1left,_))::rest671) => let val result=
MlyValue.subtract_statement((
Cobol.ADD_OR_SUBTRACT_CORRESPONDING
                (Cobol.SUBTRACT,identifier1,identifier2,rounded,
                 size_error_clauses,SUBTRACTleft,end_subtractright)
))
 in (hojfelds_NT 473,(result,SUBTRACT1left,end_subtract1right),rest671)
 end
| (874,(_,(_,_,TRACE1right))::(_,(_,READY1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 503,(result,READY1left,TRACE1right),rest671) end
| (875,(_,(_,_,TRACE1right))::(_,(_,RESET1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (hojfelds_NT 503,(result,RESET1left,TRACE1right),rest671) end
| (876,(_,(_,_,end_unstring1right))::_::_::_::_::_::_::(_,(_,
UNSTRING1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 504,(result,UNSTRING1left,end_unstring1right),rest671)
 end
| (877,(_,(_,_,delimited_by_specifications1right))::_::(_,(_,
DELIMITED1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 124,(result,DELIMITED1left,
delimited_by_specifications1right),rest671) end
| (878,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 124,(result,defaultPos,defaultPos),rest671) end
| (879,(_,(_,_,delimited_by_specifications2right))::_::(_,(_,
delimited_by_specifications1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 177,(result,delimited_by_specifications1left,
delimited_by_specifications2right),rest671) end
| (880,(_,(_,_,identifier_or_literal1right))::(_,(_,all1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 177,(result,all1left,identifier_or_literal1right),
rest671) end
| (881,(_,(_,_,into_specifications1right))::(_,(_,INTO1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 267,(result,INTO1left,into_specifications1right),
rest671) end
| (882,(_,(_,_,into_specification1right))::(_,(_,
into_specifications1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 269,(result,into_specifications1left,
into_specification1right),rest671) end
| (883,(_,(_,into_specification1left,into_specification1right))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 269,(result,into_specification1left,
into_specification1right),rest671) end
| (884,(_,(_,_,count_specification1right))::_::(_,(_,identifier1left,_
))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 268,(result,identifier1left,count_specification1right)
,rest671) end
| (885,(_,(_,_,identifier1right))::_::(_,(_,DELIMITER1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 125,(result,DELIMITER1left,identifier1right),rest671)
 end
| (886,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 125,(result,defaultPos,defaultPos),rest671) end
| (887,(_,(_,_,identifier1right))::_::(_,(_,COUNT1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 83,(result,COUNT1left,identifier1right),rest671) end
| (888,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 83,(result,defaultPos,defaultPos),rest671) end
| (889,(_,(_,_,identifier1right))::_::(_,(_,TALLYING1left,_))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 479,(result,TALLYING1left,identifier1right),rest671)
 end
| (890,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 479,(result,defaultPos,defaultPos),rest671) end
| (891,(_,(_,_,io_specification1right))::_::_::_::_::_::(_,(_,USE1left
,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 511,(result,USE1left,io_specification1right),rest671)
 end
| (892,(_,(_,_,procedure_specification1right))::_::_::_::(_,(_,
USE1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 511,(result,USE1left,procedure_specification1right),
rest671) end
| (893,(_,(_,_,PROCEDURES1right))::(_,(_,ALL1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 171,(result,ALL1left,PROCEDURES1right),rest671) end
| (894,(_,(_,_,procedure_names1right))::_::(_,(_,ALL1left,_))::rest671
) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 171,(result,ALL1left,procedure_names1right),rest671)
 end
| (895,(_,(_,procedure_names1left,procedure_names1right))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 171,(result,procedure_names1left,procedure_names1right
),rest671) end
| (896,(_,(_,EXCEPTION1left,EXCEPTION1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 160,(result,EXCEPTION1left,EXCEPTION1right),rest671)
 end
| (897,(_,(_,ERROR1left,ERROR1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 160,(result,ERROR1left,ERROR1right),rest671) end
| (898,(_,(_,file_names1left,file_names1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 275,(result,file_names1left,file_names1right),rest671)
 end
| (899,(_,(_,INPUT1left,INPUT1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 275,(result,INPUT1left,INPUT1right),rest671) end
| (900,(_,(_,OUTPUT1left,OUTPUT1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 275,(result,OUTPUT1left,OUTPUT1right),rest671) end
| (901,(_,(_,IO1left,IO1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 275,(result,IO1left,IO1right),rest671) end
| (902,(_,(_,EXTEND1left,EXTEND1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 275,(result,EXTEND1left,EXTEND1right),rest671) end
| (903,(_,(_,_,end_write1right))::_::_::_::_::_::_::_::_::(_,(_,
WRITE1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 530,(result,WRITE1left,end_write1right),rest671) end
| (904,(_,(_,_,end_write1right))::_::_::_::_::_::(_,(_,WRITE1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 530,(result,WRITE1left,end_write1right),rest671) end
| (905,(_,(_,_,identifier_or_literal1right))::_::(_,(_,FORMAT1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 531,(result,FORMAT1left,identifier_or_literal1right),
rest671) end
| (906,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 531,(result,defaultPos,defaultPos),rest671) end
| (907,(_,(_,_,identifier_or_literal1right))::_::(_,(_,TERMINAL1left,_
))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 532,(result,TERMINAL1left,identifier_or_literal1right)
,rest671) end
| (908,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 532,(result,defaultPos,defaultPos),rest671) end
| (909,(_,(_,_,identifier_or_literal1right))::_::(_,(_,STARTING1left,_
))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 533,(result,STARTING1left,identifier_or_literal1right)
,rest671) end
| (910,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 533,(result,defaultPos,defaultPos),rest671) end
| (911,(_,(_,AT1left,AT1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 536,(result,AT1left,AT1right),rest671) end
| (912,(_,(_,LINE1left,LINE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 536,(result,LINE1left,LINE1right),rest671) end
| (913,(_,(_,_,LINE1right))::(_,(_,AT1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 536,(result,AT1left,LINE1right),rest671) end
| (914,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 536,(result,defaultPos,defaultPos),rest671) end
| (915,(_,(_,_,lines_pages_specification1right))::(_,(_,advancing1left
,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 12,(result,advancing1left,
lines_pages_specification1right),rest671) end
| (916,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 12,(result,defaultPos,defaultPos),rest671) end
| (917,(_,(_,_,line_or_lines2right))::_::_::_::_::_::_::(_,(_,
ROLLING1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 534,(result,ROLLING1left,line_or_lines2right),rest671)
 end
| (918,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 534,(result,defaultPos,defaultPos),rest671) end
| (919,(_,(_,_,rolling_phrase1right))::_::(_,(_,before_after1left,_))
::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 540,(result,before_after1left,rolling_phrase1right),
rest671) end
| (920,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 540,(result,defaultPos,defaultPos),rest671) end
| (921,(_,(_,_,identifier1right))::_::(_,(_,indicator1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 535,(result,indicator1left,identifier1right),rest671)
 end
| (922,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 535,(result,defaultPos,defaultPos),rest671) end
| (923,(_,(_,IS1left,IS1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 538,(result,IS1left,IS1right),rest671) end
| (924,(_,(_,ARE1left,ARE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 538,(result,ARE1left,ARE1right),rest671) end
| (925,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 538,(result,defaultPos,defaultPos),rest671) end
| (926,(_,(_,through1left,through1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 537,(result,through1left,through1right),rest671) end
| (927,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 537,(result,defaultPos,defaultPos),rest671) end
| (928,(_,(_,_,line_or_lines1right))::(_,(_,identifier1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 296,(result,identifier1left,line_or_lines1right),
rest671) end
| (929,(_,(_,_,line_or_lines1right))::(_,(_,INTEGER1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 296,(result,INTEGER1left,line_or_lines1right),rest671)
 end
| (930,(_,(_,PAGE1left,PAGE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 296,(result,PAGE1left,PAGE1right),rest671) end
| (931,(_,(_,_,imperative_statement1right))::_::(_,(_,AT1left,_))::
rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 156,(result,AT1left,imperative_statement1right),
rest671) end
| (932,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 156,(result,defaultPos,defaultPos),rest671) end
| (933,(_,(_,ENDOFPAGE1left,ENDOFPAGE1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 140,(result,ENDOFPAGE1left,ENDOFPAGE1right),rest671)
 end
| (934,(_,(_,EOP1left,EOP1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 140,(result,EOP1left,EOP1right),rest671) end
| (935,(_,(_,_,copy_replacing_phrase1right))::_::_::(_,(_,COPY1left,_)
)::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 81,(result,COPY1left,copy_replacing_phrase1right),
rest671) end
| (936,(_,(_,_,library_name1right))::(_,(_,OF1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 292,(result,OF1left,library_name1right),rest671) end
| (937,(_,(_,_,library_name1right))::(_,(_,IN1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 292,(result,IN1left,library_name1right),rest671) end
| (938,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 292,(result,defaultPos,defaultPos),rest671) end
| (939,(_,(_,_,copy_replacing_specifications1right))::(_,(_,
REPLACING1left,_))::rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 178,(result,REPLACING1left,
copy_replacing_specifications1right),rest671) end
| (940,rest671) => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 178,(result,defaultPos,defaultPos),rest671) end
| (941,(_,(_,_,copy_replacing_specification1right))::(_,(_,
copy_replacing_specifications1left,_))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 179,(result,copy_replacing_specifications1left,
copy_replacing_specification1right),rest671) end
| (942,(_,(_,copy_replacing_specification1left,
copy_replacing_specification1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 179,(result,copy_replacing_specification1left,
copy_replacing_specification1right),rest671) end
| (943,(_,(_,_,operand2right))::_::(_,(_,operand1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (hojfelds_NT 180,(result,operand1left,operand2right),rest671) end
| (944,(_,(_,identifier1left,identifier1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 342,(result,identifier1left,identifier1right),rest671)
 end
| (945,(_,(_,literal1left,literal1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (hojfelds_NT 342,(result,literal1left,literal1right),rest671) end
| (946,(_,(_,PSEUDOTEXT1left,PSEUDOTEXT1right))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (hojfelds_NT 342,(result,PSEUDOTEXT1left,PSEUDOTEXT1right),rest671)
 end
| _ => raise Hojfeld "_" (*(mlyAction i392)*)

val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.test_cobol x => x
| _ => let exception ParseInternal
        in raise ParseInternal end) a 
