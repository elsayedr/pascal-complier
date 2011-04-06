#ifndef PPC_PROJ1_TREE_H_
#define PPC_PROJ1_TREE_H_

#include "types.h"
#include "symtab.h"

#define MAX_PARAMETER	256


/* Used for nonterminal: variable_or_function_access_maybe_assignment

   This could be any number of things: a simple variable (identifier) on
   the left-hand side of an assignment; a stand-alone procedure call (with
   no assignment); an expression involving an array index or indirection
   operator (^) indicating an l-value on the left-hand side of an assignment;
   a function name on the left-hand side of an assignment (indicating a
   return value assignment).  All uses should produce an expression, but
   the last one also requires the ST_ID.  We don't know at this production
   what will be used until we see the rest of the statement, so we use this
   structure to pass back all available relevant information.  (If the
   production is to variable_or_function_access_no_id, then the id field
   should be set to NULL).
*/
typedef struct {
    struct exprnode * expr;
    ST_ID id;
} EXPR_ID, *P_EXPR_ID;

/* Dual purpose:
     PTR_OBJ used for nonterminal: pointer_domain_type (to deal with
       possibly unresolved pointers)
     FUNC_HEAD used for nonterminal: function_heading

   For FUNC_HEAD, we can't st_install a function at the function_heading
   production, because we don't know what kind of function declaration
   it will be used for (a forward declaration, external declaration, or
   actual function definition), so we pass back the relevant information
   so that it can be installed later.
*/
typedef struct {
    ST_ID	id;
    TYPE	type;
    char	*funname;
} PTR_OBJ, *FUNC_HEAD;


/* Structures for syntax tree nodes (EXPR and EXPR_LIST) */

/* Possible expression types (tags) */
typedef enum {
    INTCONST, REALCONST, STRCONST, GID, LVAR, LFUN, NULLOP, UNOP, BINOP,
    FCALL, ERROR
} EXPR_TAG;

/* Possible nullary operators (tags) */
typedef enum {
    NULL_EOF_OP, NULL_EOLN_OP, NIL_OP
} EXPR_NULLOP;

/* Possible unary operators (tags) */
typedef enum {
    CONVERT_OP, DEREF_OP, NEG_OP, ORD_OP, CHR_OP, UN_SUCC_OP, UN_PRED_OP,
    NOT_OP, ABS_OP, SQR_OP, SIN_OP, COS_OP, EXP_OP, LN_OP, SQRT_OP, ARCTAN_OP,
    ARG_OP, TRUNC_OP, ROUND_OP, CARD_OP, ODD_OP, EMPTY_OP, POSITION_OP,
    LASTPOSITION_OP, LENGTH_OP, TRIM_OP, BINDING_OP, DATE_OP, TIME_OP,
    UN_EOF_OP, UN_EOLN_OP, INDIR_OP, UPLUS_OP, NEW_OP, DISPOSE_OP, ADDRESS_OP,
    SET_RETURN_OP
} EXPR_UNOP;

/* Possible binary operators (tags) */
typedef enum {
    ADD_OP, SUB_OP, MUL_OP, DIV_OP, MOD_OP, REALDIV_OP, EQ_OP, LESS_OP, LE_OP,
    NE_OP, GE_OP, GREATER_OP, SYMDIFF_OP, OR_OP, XOR_OP, AND_OP, BIN_SUCC_OP,
    BIN_PRED_OP, ASSIGN_OP
} EXPR_BINOP;

/* Used for lists of actual arguments to functions/procedures */
typedef struct exprlistnode {
    struct exprnode * expr;
    struct exprlistnode * next;
} EXPR_LIST_NODE, * EXPR_LIST;

/* The syntax tree node for an expression
   (includes the type of the expression)
*/
typedef struct exprnode {
    EXPR_TAG tag;
    TYPE type;
    char *name;
    union {
	long intval;
	double realval;
	char * strval;
	ST_ID gid;	    /* For global variables and global functions */
	struct {            /* For local variables and formal parameters */
	    BOOLEAN is_ref; /* TRUE if expr is a VAR (reference) parameter */
	    int offset;     /* storage location relative to frame pointer */
	    int link_count; /* Number of ref links to follow to find the var */
	} lvar;
	struct {            /* For local functions */
	    char * global_name; /* The assembler entry point label */
	    int link_count; /* Number of ref links to follow to find the fcn */
	} lfun;
	struct {            /* For nullary operators */
	    EXPR_NULLOP op;
	} nullop;
	struct {            /* For unary operators */
	    EXPR_UNOP op;
	    struct exprnode * operand;
	} unop;
	struct {            /* For binary operators */
	    EXPR_BINOP op;
	    struct exprnode * left, * right;
	} binop;
	struct {            /* For procedure and function calls */
	    struct exprnode * function;
	    EXPR_LIST args;
	} fcall;
    } u;
} EXPR_NODE, * EXPR;


/* Procedure and function prototype directives */
typedef enum { DIR_EXTERNAL, DIR_FORWARD } DIRECTIVE;


/* Records the current function identifier to detect return value assigns */
extern ST_ID func_id_stack[BS_DEPTH];
extern int fi_top;




typedef struct idlist {
    char *id;
    BOOLEAN valid;
    struct idlist *next, *prev;
}ID_T, * ID_LIST;


/*with the type and new type name, we can create and a new type to the hash table
before we create a new ID for hash, we should first check if it already exists
in the hash table, we can check that by  char *id.
*/
int tr_make_type(char *id, TYPE typeIn);

/*locate the type by its name from the hash, if it can't be found
give an error message */
TYPE tr_get_type(char *typenameIn);
TYPE tr_get_type_no_pointer(char *itm);

/*not used in this assignment*/
TYPE tr_get_const_value(char *idnameIn);

/* create different types*/
TYPE tr_create_func_pointer_type(TYPE typeIn);
TYPE tr_create_normal_pointer_type(char *typenameIn);
TYPE tr_create_func_type(TYPE typeIn, PARAM_LIST params, BOOLEAN check_args);
TYPE tr_create_proc_type(PARAM_LIST params, BOOLEAN check_args);
TYPE tr_create_subrange_type(TYPE typeIn, long low, long high);
TYPE tr_create_array_type(TYPE typeIn, INDEX_LIST indexIn);

/*call this when you finish the type defination part*/
int tr_type_resolve();

/*add new parameter to the existing list and return the new one
if paraDes == NULL, that means this parameter is the first one */
PARAM_LIST tr_add_para(PARAM_LIST paraDes, PARAM_LIST paraIn);
/* create an paramlist from the idlist, just go through all the id in the list*/
PARAM_LIST tr_create_para(ID_LIST idlistIn, char *typenameIn,  BOOLEAN is_ref);

/*add new type to the index list*/
INDEX_LIST tr_add_index(INDEX_LIST indexDes, TYPE typeIn);
/*create a index list according to existing type*/
INDEX_LIST tr_create_index(TYPE typeIn);

ID_LIST tr_add_idlist(ID_LIST idlistDes, char *idIn);
ID_LIST tr_create_idlist(char *idIn);

/*install a list of ID to hash table*/
BOOLEAN tr_install_idlist(ID_LIST idlistIn, TYPE typeIn);

/* install a function into hash table*/
FUNC_HEAD tr_install_func(char* funcname , TYPE typeIn);

BOOLEAN tr_set_func_sc(FUNC_HEAD funchead, STORAGE_CLASS sc_in);

/*add a new program to the ID list*/
int tr_add_program(char *program);

EXPR tr_make_intval_node(long intval);
EXPR tr_make_realval_node(double realval);
EXPR tr_make_strval_node(char* strval);
EXPR tr_make_gid_node(char *gid_name);
EXPR tr_make_lvar_node(BOOLEAN is_ref, int offset, int link_count);
EXPR tr_make_lfun_node(char * global_name, int link_count);
EXPR tr_make_nullop_node(EXPR_NULLOP op);
EXPR tr_make_unop_node(EXPR_UNOP op, EXPR node);
EXPR tr_make_binop_node(EXPR_BINOP op, EXPR leftnode, EXPR rightnode);
EXPR tr_make_fcall_node(EXPR_LIST args, EXPR node);
EXPR_LIST tr_make_exprlist(EXPR_LIST exprlistIn, EXPR exprIn);

TYPETAG tr_get_expr_type(EXPR exprIn);
void tr_tree_eval(EXPR exprIn);
long tr_eval_const_int(EXPR exprIn);

P_EXPR_ID tr_make_expr_id(EXPR exprIn, char* id_name, FUNC_HEAD cur_func_head);
EXPR tr_do_assignment(P_EXPR_ID p_expr_id, EXPR expr_in, FUNC_HEAD cur_func_head);

void tr_funchead_push(FUNC_HEAD head_in);
FUNC_HEAD tr_funchead_pop();
FUNC_HEAD tr_funchead_peep();
void tr_process_func_head( FUNC_HEAD funchead);
void tr_process_func_tail( FUNC_HEAD funchead);
void tr_process_local_vars(void);
void tr_process_actual_para_list(EXPR_LIST exprlistIn);
#endif /* #ifndef PPC_PROJ1_TREE_H_ */
