/*A Bison parser for the programming language Pascal.
  Copyright (C) 1989-2002 Free Software Foundation, Inc.

  Authors: Jukka Virtanen <jtv@hut.fi>
           Helsinki University of Technology
           Computing Centre
           Finland

           Peter Gerwinski <peter@gerwinski.de>
           Essen, Germany

           Bill Cox <bill@cygnus.com> (error recovery rules)

           Frank Heckenbach <frank@pascal.gnu.de>

  This file is part of GNU Pascal.

  GNU Pascal is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published
  by the Free Software Foundation; either version 1, or (at your
  option) any later version.

  GNU Pascal is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with GNU Pascal; see the file COPYING. If not, write to the
  Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
  02111-1307, USA. */

/* Bison parser for ISO 7185 Pascal originally written on
 * 3 Feb 1987 by Jukka Virtanen <jtv@hut.fi>
 *
 * Modified for use at the University of South Carolina's CSCE 531
 * (Compiler Construction) course (Spring 2005) by Stephen Fenner
 * <fenner@cse.sc.edu>
 *
 * SHIFT/REDUCE CONFLICTS
 *
 * The dangling else will not cause a shift/reduce conflict - it's
 * solved by precedence rules.
 */

%{
#include "types.h"
#include "tree.h"
#include "encode.h"
#include "backend-x86.h"

/* Cause the `yydebug' variable to be defined.  */
#define YYDEBUG 1

void set_yydebug(int);
void yyerror(const char *);

EXPR expr_temp;

/* Like YYERROR but do call yyerror */
#define YYERROR1 { yyerror ("syntax error"); YYERROR; }
%}

/* Start symbol for the grammar */

%start pascal_program

/* The union representing a semantic stack entry */
%union {
    char  *y_string;
    long   y_int;
    double y_real;
    TYPE  y_type;
    PARAM_LIST y_paralist;
    INDEX_LIST y_indexlist;
    ID_LIST y_idlist;
    STORAGE_CLASS y_storeclass;
    TYPETAG y_tapetag;
    
    EXPR y_expr;
    EXPR_LIST	y_exprlist;
    EXPR_NULLOP	y_nullop;
    EXPR_UNOP	y_unop;
    EXPR_BINOP	y_binop;
    P_EXPR_ID	y_exprid;
    FUNC_HEAD	y_funchead;
    DIRECTIVE	y_dir;
}

%token <y_string>LEX_ID

/* type defination*/
%type <y_string> typename new_identifier new_identifier_1
%type <y_string>  parameter_form
%type <y_type> type_denoter type_denoter_1 pointer_domain_type
%type <y_type> new_ordinal_type new_pointer_type new_procedural_type new_structured_type
%type <y_type> enumerated_type subrange_type
%type <y_type> functiontype array_type
%type <y_type> ordinal_index_type
%type <y_type> unpacked_structured_type

%type <y_paralist> procedural_type_formal_parameter_list procedural_type_formal_parameter
%type <y_paralist> optional_procedural_type_formal_parameter_list
%type <y_paralist> optional_par_formal_parameter_list formal_parameter_list formal_parameter
   
%type <y_indexlist> array_index_list
%type <y_idlist> id_list

%type <y_storeclass> directive_list directive

%type <y_expr> unsigned_number number constant constant_literal
%type <y_expr> expression actual_parameter static_expression
%type <y_expr> simple_expression term signed_primary primary factor
%type <y_expr> signed_factor variable_or_function_access predefined_literal
%type <y_expr> variable_or_function_access_no_as standard_functions
%type <y_expr> variable_or_function_access_no_standard_function
%type <y_expr> variable_or_function_access_no_id rest_of_statement
%type <y_expr> assignment_or_call_statement standard_procedure_statement
%type <y_expr> variable_access_or_typename optional_par_actual_parameter
%type <y_exprlist> actual_parameter_list optional_par_actual_parameter_list
%type <y_nullop> rts_fun_optpar
%type <y_unop> sign rts_fun_onepar rts_fun_parlist
%type <y_binop> relational_operator multiplying_operator adding_operator
%type <y_exprid> variable_or_function_access_maybe_assignment

%type <y_string> identifier combined_string string
%type <y_funchead> function_heading program_heading
/* Reserved words. */

/* Reserved words in Standard Pascal */
%token LEX_ARRAY LEX_BEGIN LEX_CASE LEX_CONST LEX_DO LEX_DOWNTO LEX_END
%token LEX_FILE LEX_FOR LEX_FUNCTION LEX_GOTO LEX_IF LEX_LABEL LEX_NIL
%token LEX_OF LEX_PACKED LEX_PROCEDURE LEX_PROGRAM LEX_RECORD LEX_REPEAT
%token LEX_SET LEX_THEN LEX_TO LEX_TYPE LEX_UNTIL LEX_VAR LEX_WHILE LEX_WITH
%token LEX_FORWARD

/* The following ones are not tokens used in the parser.
 * However they are used in the same context as some tokens,
 * so assign unique numbers to them.
 */
%token pp_SIN pp_COS pp_EXP pp_LN pp_SQRT pp_ARCTAN rr_POW rr_EXPON
%token r_WRITE r_READ r_INITFDR r_LAZYTRYGET r_LAZYGET r_LAZYUNGET r_POW r_EXPON
%token z_ABS z_ARCTAN z_COS z_EXP z_LN z_SIN z_SQRT z_POW z_EXPON
%token set_card set_isempty set_equal set_le set_less set_in set_clear
%token set_include set_exclude set_include_range set_copy
%token set_intersection set_union set_diff set_symdiff
%token p_DONEFDR gpc_IOCHECK gpc_RUNTIME_ERROR

/* Redefinable identifiers. */

/* Redefinable identifiers in Standard Pascal */
%token p_INPUT p_OUTPUT p_REWRITE p_RESET p_PUT p_GET p_WRITE p_READ
%token p_WRITELN p_READLN p_PAGE p_NEW p_DISPOSE
%token p_ABS p_SQR p_SIN p_COS p_EXP p_LN p_SQRT p_ARCTAN
%token p_TRUNC p_ROUND p_PACK p_UNPACK p_ORD p_CHR p_SUCC p_PRED
%token p_ODD p_EOF p_EOLN p_MAXINT p_TRUE p_FALSE

/* Additional redefinable identifiers for Borland Pascal */
%token bp_RANDOM bp_RANDOMIZE BREAK CONTINUE

/* redefinable keyword extensions */
%token RETURN_ RESULT EXIT FAIL p_CLOSE CONJUGATE p_DEFINESIZE SIZEOF
%token BITSIZEOF ALIGNOF TYPEOF gpc_RETURNADDRESS gpc_FRAMEADDRESS
%token LEX_LABEL_ADDR

/* GPC internal tokens */
%token LEX_INTCONST LEX_STRCONST LEX_REALCONST
%token LEX_RANGE LEX_ELLIPSIS

/* We don't declare precedences for operators etc. We don't need
   them since our rules define precedence implicitly, and too many
   precedences increase the chances of real conflicts going unnoticed. */
%token LEX_ASSIGN
%token '<' '=' '>' LEX_IN LEX_NE LEX_GE LEX_LE
%token '-' '+' LEX_OR LEX_OR_ELSE LEX_CEIL_PLUS LEX_CEIL_MINUS LEX_FLOOR_PLUS LEX_FLOOR_MINUS
%token '/' '*' LEX_DIV LEX_MOD LEX_AND LEX_AND_THEN LEX_SHL LEX_SHR LEX_XOR LEX_CEIL_MULT LEX_CEIL_DIV LEX_FLOOR_MULT LEX_FLOOR_DIV
%token LEX_POW LEX_POWER LEX_IS LEX_AS
%token LEX_NOT

/* Various extra tokens */
%token LEX_EXTERNAL ucsd_STR p_MARK p_RELEASE p_UPDATE p_GETTIMESTAMP p_UNBIND
%token p_EXTEND bp_APPEND p_BIND p_SEEKREAD p_SEEKWRITE p_SEEKUPDATE LEX_SYMDIFF
%token p_ARG p_CARD p_EMPTY p_POSITION p_LASTPOSITION p_LENGTH p_TRIM p_BINDING
%token p_DATE p_TIME LEX_RENAME LEX_IMPORT LEX_USES LEX_QUALIFIED LEX_ONLY



/* Precedence rules */

/* The following precedence declarations are just to avoid the dangling
   else shift-reduce conflict. We use prec_if rather than LEX_IF to
   avoid possible conflicts elsewhere involving LEX_IF going unnoticed. */
%nonassoc prec_if
%nonassoc LEX_ELSE

/* These tokens help avoid S/R conflicts from error recovery rules. */
%nonassoc lower_than_error
%nonassoc error

%%

/* Pascal parser starts here */

pascal_program:
    /* empty */
  {}| program_component_list
  {};

program_component_list:
    program_component
  {}| program_component_list program_component
  {};

program_component:
    main_program_declaration '.'
  {};

main_program_declaration:
    program_heading semi import_or_any_declaration_part statement_part
  {
  	//get out of the func
//  	b_func_epilogue("main");
	gen_process_prgm_tail( $1 );
  	tr_funchead_pop();
  };

program_heading:
    LEX_PROGRAM new_identifier optional_par_id_list
  {
//  	tr_add_program($2);
	$$ = tr_install_func($2 , tr_create_func_type(NULL, NULL, TRUE));
  	tr_funchead_push($$);
  };

optional_par_id_list:
    /* empty */
  {}| '(' id_list ')'
  {};
  
  
//edited by ygao   3/22/2011 9:59AM
//edited by ygao   3/22/2011 5:46PM
id_list:
    new_identifier
  {
		$$ = tr_create_idlist($1);
		
  }
  | id_list ',' new_identifier
  {
		$$ = tr_add_idlist($1, $3);
  };

typename:
    LEX_ID
  {};

identifier:
    LEX_ID
  {
  	$$ = yylval.y_string;
  };

new_identifier:
    new_identifier_1
  {};

new_identifier_1:
    LEX_ID
/* Standard Pascal constants */
  {}| p_MAXINT
  {}| p_FALSE
  {}| p_TRUE
/* Standard Pascal I/O */
  {}| p_INPUT
  {}| p_OUTPUT
  {}| p_REWRITE
  {}| p_RESET
  {}| p_PUT
  {}| p_GET
  {}| p_WRITE
  {}| p_READ
  {}| p_WRITELN
  {}| p_READLN
  {}| p_PAGE
  {}| p_EOF
  {}| p_EOLN
/* Standard Pascal heap handling */
  {}| p_NEW
  {}| p_DISPOSE
/* Standard Pascal arithmetic */
  {}| p_ABS
  {}| p_SQR
  {}| p_SIN
  {}| p_COS
  {}| p_EXP
  {}| p_LN
  {}| p_SQRT
  {}| p_ARCTAN
  {}| p_TRUNC
  {}| p_ROUND
/* Standard Pascal transfer functions */
  {}| p_PACK
  {}| p_UNPACK
/* Standard Pascal ordinal functions */
  {}| p_ORD
  {}| p_CHR
  {}| p_SUCC
  {}| p_PRED
  {}| p_ODD
/* Other extensions */
  {}| BREAK
  {}| CONTINUE
  {}| RETURN_
  {}| RESULT
  {}| EXIT
  {}| FAIL
  {}| SIZEOF
  {}| BITSIZEOF
  {};

import_or_any_declaration_part:
    any_declaration_import_part
  {
    	//after declearation, generate main here
//  	b_func_prologue("main");
  	gen_process_prgm_head( tr_funchead_peep());
  };

any_declaration_import_part:
    /* empty */
  {}| any_declaration_import_part any_or_import_decl
  {};

any_or_import_decl:
    import_part
  {}| any_decl
  {};

any_declaration_part:
    /* empty */
  {
  	if( tr_funchead_peep() != NULL )
  	{
		gen_process_func_head( tr_funchead_peep());
		gen_process_local_vars();
	}
  }| any_declaration_part any_decl
  {
  	if( tr_funchead_peep() != NULL )
  	{
		gen_process_func_head( tr_funchead_peep());
		gen_process_local_vars();
	}
  };

any_decl:
    simple_decl
  {
  }| function_declaration
  {};

simple_decl:
    label_declaration_part
  {}| constant_definition_part
  {}| type_definition_part
  {}| variable_declaration_part
  {
  	
  };

/* Label declaration part */

label_declaration_part:
    LEX_LABEL label_list semi
  {};

label_list:
    label
  {}| label_list ',' label
  {};

/* Labels are returned as identifier nodes for compatibility with gcc */
label:
    LEX_INTCONST
  {}| new_identifier
  {};

/* constant definition part */

constant_definition_part:
    LEX_CONST constant_definition_list
  {};

constant_definition_list:
    constant_definition
  {}| constant_definition_list constant_definition
  {};

constant_definition:
    new_identifier '=' static_expression semi
  {};

//edited by ygao   3/22/2011 3:09PM
//I don't touch the number part, since we just use the unsigned number
//for this time.
constant:
    identifier
  {}| sign identifier
  {}| number
  {
  }| constant_literal
  {};

number:
    sign unsigned_number
  {
  	$$ = tr_make_unop_node($1, $2);
  }| unsigned_number
  {};

unsigned_number:
    LEX_INTCONST
  {
  	$$ = tr_make_intval_node(yylval.y_int);
  }
  | LEX_REALCONST
  {
  	$$ = tr_make_realval_node(yylval.y_real);
  };

sign:
    '+'
  {
  }| '-'
  {
  	$$ = NEG_OP;
  };

constant_literal:
    combined_string
  {
  	$$ = tr_make_strval_node($1);
  }| predefined_literal
  {};

predefined_literal:
    LEX_NIL
  {
  	$$ = tr_make_intval_node(0);
  }
  | p_FALSE
  {
  	$$ = tr_make_intval_node(0);
  }
  | p_TRUE
  {
  	$$ = tr_make_intval_node(1);
  };

combined_string:
    string
  {};

string:
    LEX_STRCONST
  {
  	$$ = yylval.y_string;
  }| string LEX_STRCONST
  {};

type_definition_part:
    LEX_TYPE type_definition_list semi
  {
  	tr_type_resolve();
  };

type_definition_list:
    type_definition
  {}| type_definition_list semi type_definition
  {};

type_definition:
    new_identifier '=' type_denoter
  {
  	tr_make_type($1, $3);
  	//st_dump();
  	//printf("we have a new type.\n");
  };

type_denoter:
    typename
  {
  	$$ = tr_get_type_no_pointer($1);
  }
  | type_denoter_1
  {};

type_denoter_1:
    new_ordinal_type
  {}| new_pointer_type
  {}| new_procedural_type
  {}| new_structured_type
  {};

new_ordinal_type:
    enumerated_type
  {}| subrange_type
  {};

enumerated_type:
    '(' enum_list ')'
  {};

enum_list:
    enumerator
  {}| enum_list ',' enumerator
  {};

enumerator:
    new_identifier
  {};

//edited by ygao   3/22/2011 10:37AM
subrange_type:
    constant LEX_RANGE constant
  {
  	$$ = tr_create_subrange_type(NULL, tr_eval_const_int($1), tr_eval_const_int($3));
  };

//edited by ygao   
new_pointer_type:
    pointer_char pointer_domain_type
  {
  	$$ = $2;
  };

pointer_char:
    '^'
  {}| '@'
  {};

//edited by ygao   3/22/2011 10:06AM
pointer_domain_type:
   new_identifier
  {
  	  $$ = tr_create_normal_pointer_type($1);
  	  
  }
  | new_procedural_type
  {
  	$$ = tr_create_func_pointer_type($1);
  };

new_procedural_type:
    LEX_PROCEDURE optional_procedural_type_formal_parameter_list
  {
  	$$ = tr_create_func_type(NULL, $2, TRUE);
  }
  | LEX_FUNCTION optional_procedural_type_formal_parameter_list functiontype
  {
  	$$ = tr_create_func_type($3, $2, TRUE);
  };

optional_procedural_type_formal_parameter_list:
    /* empty */
  {
  	// no paramter
  	$$ = NULL;
  }| '(' procedural_type_formal_parameter_list ')'
  {
  	$$ = $2;
  };

//edited by ygao   3/22/2011 10:06AM
procedural_type_formal_parameter_list:
    procedural_type_formal_parameter
  {
  	$$ = $1;
  }
  | procedural_type_formal_parameter_list semi procedural_type_formal_parameter
  {
  	$$ = tr_add_para($1, $3);
  };

//edited by ygao   3/22/2011 10:02AM
procedural_type_formal_parameter:
    id_list
  {
  	//$$ = tr_create_para($1, NULL,  FALSE);
  }
  | id_list ':' typename
  {
  	$$ = tr_create_para($1, $3, FALSE);
  }
  | LEX_VAR id_list ':' typename
  {
  	$$ = tr_create_para($2, $4, TRUE);
  }
  | LEX_VAR id_list
  {
  	//$$ = tr_create_para($2, NULL, TRUE);
  };

new_structured_type:
    LEX_PACKED unpacked_structured_type
  {}| unpacked_structured_type
  {};

unpacked_structured_type:
    array_type
  {}| file_type
  {}| set_type
  {}| record_type
  {};

/* Array */
//edited by ygao   3/22/2011 10:44AM
array_type:
    LEX_ARRAY '[' array_index_list ']' LEX_OF type_denoter
  {
  	$$ = tr_create_array_type($6, $3);
  };

//edited by ygao   3/22/2011 10:46AM
array_index_list:
    ordinal_index_type
  {
  	$$ = tr_create_index($1);
  }
  | array_index_list ',' ordinal_index_type
  {
  	$$ = tr_add_index($1, $3);
  };

//edited by ygao   3/22/2011 10:46AM
ordinal_index_type:
    new_ordinal_type
  {}| typename
  {
  	$$ = tr_get_type($1);
  };

/* FILE */
file_type:
    LEX_FILE direct_access_index_type LEX_OF type_denoter
  {};

direct_access_index_type:
    /* empty */
  {}| '[' ordinal_index_type ']'
  {};


/* sets */
set_type:
    LEX_SET LEX_OF type_denoter
  {};

record_type:
    LEX_RECORD record_field_list LEX_END
  {};

record_field_list:
    /* empty */
  {}| fixed_part optional_semicolon
  {}| fixed_part semi variant_part
  {}| variant_part
  {};

fixed_part:
    record_section
  {}| fixed_part semi record_section
  {};

record_section:
    id_list ':' type_denoter
  {};

variant_part:
    LEX_CASE variant_selector LEX_OF variant_list rest_of_variant
  {};

rest_of_variant:
    optional_semicolon
  {}| case_default '(' record_field_list ')' optional_semicolon
  {};

variant_selector:
    new_identifier ':' variant_type
  {}| variant_type
  {};

variant_type:
    typename
  {}| new_ordinal_type
  {};

variant_list:
    variant
  {}| variant_list semi variant
  {};

variant:
    case_constant_list ':' '(' record_field_list ')'
  {};

case_constant_list:
    one_case_constant
  {}| case_constant_list ',' one_case_constant
  {};

one_case_constant:
    static_expression
  {}| static_expression LEX_RANGE static_expression
  {};

/* variable declaration part */

variable_declaration_part:
    LEX_VAR variable_declaration_list
  {};

variable_declaration_list:
    variable_declaration
  {}| variable_declaration_list variable_declaration
  {};

//edited by ygao   3/22/2011 6:22PM
variable_declaration:
    id_list ':' type_denoter semi
  {
  	tr_install_idlist($1, $3);
  	gen_var_asm($1);
  };

function_declaration:
    function_heading semi directive_list semi
  {
  	tr_funchead_pop();
  	tr_set_func_sc($1, $3);
  }
  | function_heading semi any_declaration_part statement_part semi
  {
  	
  	gen_process_func_tail( $1 );
  	tr_funchead_pop();
  };

function_heading:
    LEX_PROCEDURE new_identifier optional_par_formal_parameter_list
  {
  	$$ = tr_install_func($2 , tr_create_func_type(NULL, $3, TRUE));
  	tr_funchead_push($$);
  }
  | LEX_FUNCTION new_identifier optional_par_formal_parameter_list functiontype
  {
  	$$ = tr_install_func($2 , tr_create_func_type($4, $3, TRUE));
  	tr_funchead_push($$);
  };

directive_list:
    directive
  {}
  | directive_list semi directive
  {
  };

directive:
    LEX_FORWARD
  {
  	$$ = STATIC_SC;
  }
  | LEX_EXTERNAL
  {
  	$$ = EXTERN_SC;
  };

functiontype:
    /* empty */
  {}| ':' typename
  {
  	$$ = tr_get_type($2);
  };

/* parameter specification section */

optional_par_formal_parameter_list:
    /* empty */
  {
//  	printf("QQQQQQQQQQQQQQQQQ empty para\n");
  	$$ = NULL;
  }
  | '(' formal_parameter_list ')'
  {
  	$$ = $2;
  };

formal_parameter_list:
    formal_parameter
  {
  	$$ = $1;
  }
  | formal_parameter_list semi formal_parameter
  {
  	$$ = tr_add_para($1, $3);
  };

formal_parameter:
    id_list ':' parameter_form
  {
  	$$ = tr_create_para($1, $3, FALSE);
  }
  | LEX_VAR id_list ':' parameter_form
  {
  	$$ = tr_create_para($2, $4, TRUE);
  }
  | function_heading
  {}| id_list ':' conformant_array_schema
  {}| LEX_VAR id_list ':' conformant_array_schema
  {};

parameter_form:
    typename
  {}| open_array
  {};

conformant_array_schema:
    packed_conformant_array_schema
  {}| unpacked_conformant_array_schema
  {};

typename_or_conformant_array_schema:
    typename
  {}| packed_conformant_array_schema
  {}| unpacked_conformant_array_schema
  {};

packed_conformant_array_schema:
    LEX_PACKED LEX_ARRAY '[' index_type_specification ']' LEX_OF typename_or_conformant_array_schema
  {};

unpacked_conformant_array_schema:
    LEX_ARRAY '[' index_type_specification_list ']' LEX_OF typename_or_conformant_array_schema
  {};

index_type_specification:
    new_identifier LEX_RANGE new_identifier ':' typename
  {};

index_type_specification_list:
    index_type_specification
  {}| index_type_specification_list semi index_type_specification
  {};

open_array:
    LEX_ARRAY LEX_OF typename
  {};

statement_part:
    compound_statement
  {};

compound_statement:
    LEX_BEGIN statement_sequence LEX_END
  {
//  	printf("#####################\n");
  };

statement_sequence:
    statement
  {}| statement_sequence semi statement
  {};

statement:
    label ':' unlabelled_statement
  {}| unlabelled_statement
  {};

unlabelled_statement:
    structured_statement
  {}| simple_statement
  {};

structured_statement:
    compound_statement
  {}| with_statement
  {}| conditional_statement
  {}| repetitive_statement
  {};

with_statement:
    LEX_WITH structured_variable_list LEX_DO statement
  {};

structured_variable_list:
    structured_variable
  {}| structured_variable_list ',' structured_variable
  {};

structured_variable:
    variable_or_function_access
  {};

conditional_statement:
    if_statement
  {}| case_statement
  {};

simple_if:
    LEX_IF boolean_expression LEX_THEN statement
  {};

if_statement:
    simple_if LEX_ELSE statement
  {}| simple_if %prec prec_if
  {};

case_statement:
    LEX_CASE expression LEX_OF case_element_list optional_semicolon_or_else_branch LEX_END
  {};

optional_semicolon_or_else_branch:
    optional_semicolon
  {}| case_default statement_sequence
  {};

case_element_list:
    case_element
  {}| case_element_list semi case_element
  {};

case_element:
    case_constant_list ':' statement
  {};

case_default:
    LEX_ELSE
  {}| semi LEX_ELSE
  {};

repetitive_statement:
    repeat_statement
  {}| while_statement
  {}| for_statement
  {};

repeat_statement:
    LEX_REPEAT statement_sequence LEX_UNTIL boolean_expression
  {};

while_statement:
    LEX_WHILE boolean_expression LEX_DO statement
  {};

for_statement:
    LEX_FOR variable_or_function_access LEX_ASSIGN expression for_direction expression LEX_DO statement
  {};

for_direction:
    LEX_TO
  {}| LEX_DOWNTO
  {};

simple_statement:
    empty_statement
  {}| goto_statement
  {}| assignment_or_call_statement
  {}| standard_procedure_statement
  {}| statement_extensions
  {};

empty_statement:
    /* empty */ %prec lower_than_error
  {};

goto_statement:
    LEX_GOTO label
  {};

/* function calls */

optional_par_actual_parameter_list:
    /* empty */
  {
//  	gen_process_actual_para_list(NULL);
  }| '(' actual_parameter_list ')'
  {
  	//printf("$$$$$$$$$$$$$$$$\n");
//    gen_process_actual_para_list($2);
  };

actual_parameter_list:
    actual_parameter
  {
  //printf("@@@@@@@@@@@@@@@@@@@@@@@2");
  	fflush(stdout);
  	$$ = tr_make_exprlist(NULL, $1);
  	//printf("@@@@@@@@@@@@@@@@@@@@@1");
  }
  | actual_parameter_list ',' actual_parameter
  {
  
  	fflush(stdout);
  	$$ = tr_make_exprlist($1, $3);
  };

actual_parameter:
    expression
  {
  };

/* ASSIGNMENT and procedure calls */

assignment_or_call_statement:
    variable_or_function_access_maybe_assignment rest_of_statement
  {
  		if($2 != NULL)
  		{
//  			gen_tree_asm($1->expr);
//printf("1::::::::::::::::::");
			if($1 != NULL)
			{
    			if(tr_check_assignment($1->expr) == TRUE)
    			{
      				gen_tree_asm($2);
      				$$ = gen_do_assignment($1, $2, tr_funchead_peep());
      			}
      		}
  		}

  		else
  		{
//  			printf("2::::::::::::::::::");
				
			if( $1 != NULL )
			{
				tr_check_proc($1->expr);
  				gen_tree_asm($1->expr);
  			}
  		}
  };

variable_or_function_access_maybe_assignment:
    identifier
  {
//  	printf(">>>>>>>>>>>id access\n");
  	fflush(stdout);
  	$$ = tr_make_expr_id( NULL, $1, tr_funchead_peep());
  }
  | address_operator variable_or_function_access
  {}| variable_or_function_access_no_id
  {
//  	printf(">>>>>>>>>>>no id access\n");
  	fflush(stdout);
  	$$ = tr_make_expr_id( $1, NULL, tr_funchead_peep());
  };

rest_of_statement:
    /* Empty */
  {
  	$$ = NULL;
  }| LEX_ASSIGN expression
  {
	$$ = $2;
  };

standard_procedure_statement:
    rts_proc_onepar '(' actual_parameter ')'
  {
//    	printf("1######################3");
  	fflush(stdout);
  }| rts_proc_parlist '(' actual_parameter_list ')'
  {
//    	printf("2######################3");
  	fflush(stdout);
  }| p_WRITE optional_par_write_parameter_list
  {}| p_WRITELN optional_par_write_parameter_list
  {}| p_READ optional_par_actual_parameter_list
  {}| p_READLN optional_par_actual_parameter_list
  {}| p_PAGE optional_par_actual_parameter_list
  {}| ucsd_STR '(' write_actual_parameter_list ')'
  {}| p_DISPOSE '(' actual_parameter ')'
  {
//  	printf("@@@@@@@@@@@@@@@@@@@2\n");
  	$$ = tr_make_unop_node(DISPOSE_OP, $3);
  	gen_tree_asm($$);
  }| p_DISPOSE '(' actual_parameter ',' actual_parameter_list ')'
  {
//  	$$ = tr_make_exprlist($5, $3);
  };

optional_par_write_parameter_list:
    /* empty */
  {}| '(' write_actual_parameter_list ')'
  {};

write_actual_parameter_list:
    write_actual_parameter
  {}| write_actual_parameter_list ',' write_actual_parameter
  {};

write_actual_parameter:
    actual_parameter
  {}| actual_parameter ':' expression
  {}| actual_parameter ':' expression ':' expression
  {};

/* run time system calls with one parameter */
rts_proc_onepar:
    p_PUT
  {}| p_GET
  {}| p_MARK
  {}| p_RELEASE
  {}| p_CLOSE
  {}| p_UPDATE
  {}| p_GETTIMESTAMP
  {}| p_UNBIND
  {};

rts_proc_parlist:
    p_REWRITE     /* Up to three args */
  {}| p_RESET       /* Up to three args */
  {}| p_EXTEND      /* Up to three args */
  {}| bp_APPEND     /* Up to three args */
  {}| p_PACK        /* Three args */
  {}| p_UNPACK      /* Three args */
  {}| p_BIND        /* Two args */
  {}| p_SEEKREAD
  {}| p_SEEKWRITE
  {}| p_SEEKUPDATE
  {}| p_DEFINESIZE  /* Two args */
  {}| LEX_AND           /* Two args */
  {}| LEX_OR            /* Two args */
  {}| LEX_NOT           /* One arg */
  {}| LEX_XOR        /* Two args */
  {}| LEX_SHL           /* Two args */
  {}| LEX_SHR           /* Two args */
  {};

statement_extensions:
    return_statement
  {}| continue_statement
  {}| break_statement
  {};

return_statement:
    RETURN_
  {}| RETURN_ expression
  {
  }| EXIT
  {}| FAIL
  {};

break_statement:
    BREAK
  {};

continue_statement:
    CONTINUE
  {};

variable_access_or_typename:
    variable_or_function_access_no_id
  {}| LEX_ID
  {
//  	printf("@@@@@@@@@@@id access\n");
  	$$ = tr_make_gid_node(yylval.y_string);

  };

index_expression_list:
    index_expression_item
  {}| index_expression_list ',' index_expression_item
  {};

index_expression_item:
    expression
  {
  }| expression LEX_RANGE expression
  {};

/* expressions */

static_expression:
    expression
  {
  };

boolean_expression:
    expression
  {
  };

expression:
    expression relational_operator simple_expression
  {
  	$$ = tr_make_binop_node($2, $1, $3);
  }| expression LEX_IN simple_expression
  {}| simple_expression
  {
  		//printf("@@@@@@@@@@@@");
  };

simple_expression:
    term
  {}| simple_expression adding_operator term
  {
  	$$ = tr_make_binop_node($2, $1, $3);
  }| simple_expression LEX_SYMDIFF term
  {}| simple_expression LEX_OR term
  {}| simple_expression LEX_XOR term
  {};

term:
    signed_primary
  {}| term multiplying_operator signed_primary
  {
  	$$ = tr_make_binop_node($2, $1, $3);
  }| term LEX_AND signed_primary
  {};

signed_primary:
    primary
  {}| sign signed_primary
  {
  $$ = tr_make_unop_node($1, $2);
  };

primary:
    factor
  {}| primary LEX_POW factor
  {}| primary LEX_POWER factor
  {}| primary LEX_IS typename
  {};

signed_factor:
    factor
  {}| sign signed_factor
  {
  	$$ = tr_make_unop_node($1, $2);
  };

factor:
    variable_or_function_access
  {
  	//printf("$$$$$$$$$$$$$function access\n");
  	  	//gen_tree_asm($1);
  }| constant_literal
  {
  	//gen_tree_asm($1);
  }| unsigned_number
  {
  	//gen_tree_asm($1);
  }| set_constructor
  {}| LEX_NOT signed_factor
  {}| address_operator factor
  {};

address_operator:
    '@'
  {};

variable_or_function_access:
    variable_or_function_access_no_as
  {}| variable_or_function_access LEX_AS typename
  {};

variable_or_function_access_no_as:
    variable_or_function_access_no_standard_function
  {}| standard_functions
  {};

variable_or_function_access_no_standard_function:
    identifier
  {
//  	printf("@@@@@@@@@@@no standard func\n");
  	$$ = tr_make_gid_node($1);
  }| variable_or_function_access_no_id
  {};

variable_or_function_access_no_id:
    p_OUTPUT
  {}| p_INPUT
  {}| variable_or_function_access_no_as '.' new_identifier
  {}| '(' expression ')'
  {
  	$$ = $2;
  }| variable_or_function_access pointer_char
  {
  	$$ = tr_make_unop_node(DEREF_OP, $1);
  }| variable_or_function_access '[' index_expression_list ']'
  {}| variable_or_function_access_no_standard_function '(' actual_parameter_list ')'
  {
//    	printf("4######################3");
//  		fflush(stdout);
  	 gen_process_actual_para_list($3);
  }| p_NEW '(' variable_access_or_typename ')'
  {
//  	printf("~~~~~~~~~~~~~~~~~~~~~~~~~");
  	$$ = tr_make_unop_node(NEW_OP, $3);
//  	gen_tree_asm($$);
  };

set_constructor:
    '[' ']'
  {}| '[' set_constructor_element_list ']'
  {};

set_constructor_element_list:
    member_designator
  {}| set_constructor_element_list ',' member_designator
  {};

member_designator:
    expression
  {
  }| expression LEX_RANGE expression
  {};

standard_functions:
    rts_fun_onepar '(' actual_parameter ')'
  {
  	$$ = tr_make_unop_node($1, $3);
  }| rts_fun_optpar optional_par_actual_parameter
  {}| rts_fun_parlist '(' actual_parameter_list ')'
  {
//  	printf("######################3");
  	fflush(stdout);
  	//do consider the multi paralist
  	$$ = tr_make_unop_node($1, $3->expr);
  };

optional_par_actual_parameter:
    /* empty */
  {}|  '(' actual_parameter ')'
  {};

rts_fun_optpar:
    p_EOF
  {}| p_EOLN
  {};
    
rts_fun_onepar:
    p_ABS
  {
  	$$ = ABS_OP;
  }| p_SQR
  {
  	$$ = SQR_OP;
  }| p_SIN
  {
  	$$ = SIN_OP;
  }| p_COS
  {
  	$$ = COS_OP;
  }| p_EXP
  {
  	$$= EXP_OP;
  }| p_LN
  {
  	$$ = LN_OP;
  }| p_SQRT
  {
  	$$ = SQRT_OP;
  }| p_ARCTAN
  {
  	$$ = ARCTAN_OP;
  }| p_ARG
  {
  	$$ = ARG_OP;
  }| p_TRUNC
  {
  	$$ = TRUNC_OP;
  }| p_ROUND
  {
  	$$ = ROUND_OP;
  }| p_CARD
  {
  	$$ = CARD_OP;
  }| p_ORD
  {
  	$$ = ORD_OP;
  }| p_CHR
  {
  	$$ = CHR_OP;
  }| p_ODD
  {
  	$$ = ODD_OP;
  }| p_EMPTY
  {
  	$$ = EMPTY_OP;
  }| p_POSITION
  {
  	$$ = POSITION_OP;
  }| p_LASTPOSITION
  {
  	$$ = LASTPOSITION_OP;
  }| p_LENGTH
  {
  	$$ = LENGTH_OP;
  }| p_TRIM
  {
  	$$ = TRIM_OP;
  }| p_BINDING
  {
  	$$ = BINDING_OP;
  }| p_DATE
  {
  	$$ = DATE_OP;
  }| p_TIME
  {
  	$$ = TIME_OP;
  };

rts_fun_parlist:
    p_SUCC        /* One or two args */
  {
  	$$ = UN_SUCC_OP;
  }| p_PRED        /* One or two args */
  {
  	$$ = UN_PRED_OP;
  };

relational_operator:
    LEX_NE
  {
  	$$ = NE_OP;
  }| LEX_LE
  {
  	$$ = LE_OP;
  }
  | LEX_GE
  {
  	$$ = GE_OP;
  }| '='
  {
  	$$ = EQ_OP;
  }| '<'
  {
  	$$ = LESS_OP;
  }| '>'
  {
  	$$ = GREATER_OP;
  };

multiplying_operator:
    LEX_DIV
  {
  	$$ = DIV_OP;
  }| LEX_MOD
  {
  $$ = MOD_OP;
  }| '/'
  {
  	//$$ = DIV_OP;
  }| '*'
  {
  	$$ = MUL_OP;
  };

adding_operator:
    '-'
  {
  $$ = SUB_OP;
  }| '+'
  {
  $$ = ADD_OP;
  };

semi:
    ';'
  {};

optional_semicolon:
    /* empty */
  {}| ';'
  {};

optional_rename:
    /* empty */
  {}| LEX_RENAME new_identifier
  {};

import_part:
    LEX_IMPORT import_specification_list semi
  {}| LEX_USES uses_list semi
  {};

import_specification_list:
    import_specification
  {}| import_specification_list semi import_specification
  {};

uses_list:
    import_specification
  {}| uses_list ',' import_specification
  {};

import_specification:
    new_identifier optional_access_qualifier optional_import_qualifier optional_unit_filename
  {};

optional_access_qualifier:
    /* Empty */
  {}| LEX_QUALIFIED
  {};

optional_import_qualifier:
    /* Empty */
  {}| '(' import_clause_list ')'
  {}| LEX_ONLY '(' import_clause_list ')'
  {};

optional_unit_filename:
    /* Empty */
  {}| LEX_IN combined_string
  {};

import_clause_list:
    import_clause
  {}| import_clause_list ',' import_clause
  {};

import_clause:
    new_identifier optional_rename
  {};

%%

void yyerror(const char *msg)
{
    error(msg);
}

/* Sets the value of the 'yydebug' variable to VALUE.
   This is a function so we don't have to have YYDEBUG defined
   in order to build the compiler.  */
void
set_yydebug (value)
     int value;
{
#if YYDEBUG != 0
  yydebug = value;
#else
  warning ("YYDEBUG not defined.");
#endif
}
