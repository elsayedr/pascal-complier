#include "stdlib.h"
#include "symtab.h"
#include "types.h"
#include "tree.h"
#include "message.h"
#include "backend-x86.h"
#include "stdio.h"


TYPE type_judege(TYPE left, TYPE right)
{
	if(ty_query(left) == TYDOUBLE)
		return left;
	if(ty_query(right) == TYDOUBLE)
		return right;
	if(ty_query(left) == TYFLOAT)
		return left;
	if(ty_query(right) == TYFLOAT)
		return right;
	if(ty_query(left) == TYSIGNEDLONGINT)
		return left;
	if(ty_query(right) == TYSIGNEDLONGINT)
		return right;
		
	return left;
}

BOOLEAN check_simple_type(TYPE typeIn)
{
	if(typeIn == NULL)
		return FALSE;
	TYPETAG curtag = ty_query(typeIn);
	if( curtag != TYVOID && curtag != TYFUNC && curtag != TYERROR
		&& curtag != TYARRAY)
		return TRUE;
	return FALSE;
}

BOOLEAN check_data_type(TYPE typeIn)
{
	if(typeIn == NULL)
		return FALSE;
	TYPETAG curtag = ty_query(typeIn);
	if( curtag != TYVOID && curtag != TYFUNC && curtag != TYERROR)
		return TRUE;
	return FALSE;
}

//get type by its bucket record
TYPE get_type_by_id(ST_ID bucket_rec)
{
	ST_DR stack_rec;
	int block;
	if( bucket_rec != NULL)
	{
		stack_rec =  st_lookup(bucket_rec, &block);
		if( stack_rec != NULL)
		{
//			if( stack_rec->tag != TYPENAME)
//				warning("the stack record found is not a type\n");
			return stack_rec->u.typename.type;
		}
		else
		{
//			warning("can't find that data record in the stack\n");
		}
	}
	else
	{
		warning("can't find that type in the bucket\n");
	}
	return NULL;
}

//add the type generated to stack
int tr_make_type(char *id, TYPE typeIn)
{
	int ret;
	ST_DR dr;
	if( id == NULL )
	{
		error("Input id is NULL");
	}
	if( typeIn == NULL )
	{
//		error("Input type is NULL");
	}
	
	dr = stdr_alloc();
	dr->tag = TYPENAME;
	if(typeIn == NULL)
		dr->u.typename.type = ty_build_basic(TYERROR);
	else
		dr->u.typename.type = typeIn;
	ret = st_install(st_enter_id(id),dr);
	if( ret == FALSE )
	{
//		warning ( "type already existed" );
		stdr_free(dr);
	}
	return 0;
}

// get type by its name
// for non-pointer type, use tr_get_type_no_pointer
TYPE tr_get_type(char *itm)
{
	ST_ID bucket_rec;
	ST_DR stack_rec;
	int block;
	//look for record on hash
	//if it does not exit, create one hash record
	//if yes, go for it on the stack
	bucket_rec = st_lookup_id(itm);
	if( bucket_rec != NULL)
	{
		stack_rec =  st_lookup(bucket_rec, &block);
		if( stack_rec != NULL)
		{
//			if( stack_rec->tag != TYPENAME)
//				warning("the stack record found is not a type\n");
			return stack_rec->u.typename.type;
		}
		else
		{
			error("Undeclared type name: \"%s\"", itm);
		}
	}
	else
	{
		bucket_rec = st_enter_id(itm);
	}
	return NULL;
}

//for pointer, the type can be refered before it has been created.
//if it's a non-pointer type ,use this
TYPE tr_get_type_no_pointer(char *itm)
{
	ST_ID bucket_rec;
	ST_DR stack_rec;
	int block;
	bucket_rec = st_lookup_id(itm);
	if( bucket_rec != NULL)
	{
		stack_rec =  st_lookup(bucket_rec, &block);
		if( stack_rec != NULL)
		{
//			if( stack_rec->tag != TYPENAME)
//				warning("the stack record found is not a type\n");
			return stack_rec->u.typename.type;
		}
		else
		{
			error("Undeclared type name: \"%s\"", itm);
		}
	}
	else
	{
		bucket_rec = st_enter_id(itm);
		error("Undeclared type name: \"%s\"", itm);
	}
	return NULL;
}

//for function and non-function pointer type
//the difference is the input is a real type or just the type name
TYPE tr_create_func_pointer_type(TYPE typeIn)
{
	if(typeIn != NULL)
	{
		return ty_build_ptr(NULL, typeIn);
	}
	error("type input is NULL(tr_create_func_pointer_type)");
	return NULL;
}

TYPE tr_create_normal_pointer_type(char *typenameIn)
{
	ST_ID bucket_rec;
	TYPE typeIn;
	typeIn = tr_get_type(typenameIn);
	if(typenameIn != NULL)
	{
		bucket_rec = st_enter_id(typenameIn);
		if( typeIn != NULL)
			return ty_build_ptr(NULL, typeIn);
		else
		{
			return ty_build_ptr(bucket_rec, NULL);
		}
	}
	error("type name input is NULL");
	return NULL;
}

TYPE tr_create_proc_type(PARAM_LIST params, BOOLEAN check_args)
{
	return ty_build_func(NULL, params, check_args);
}

TYPE tr_create_func_type(TYPE typeIn, PARAM_LIST params, BOOLEAN check_args)
{
	TYPE type;
	if(typeIn == NULL)
		type = ty_build_basic(TYVOID);
	else
		type = 	typeIn;
	if(check_simple_type(type) == FALSE && typeIn != NULL)
		error("Function return type must be simple type");
//	printf("~~~~~~~~~~~~~~~`type:%d\n", ty_query(type));
	return ty_build_func(type, params, check_args);
}

TYPE tr_create_subrange_type(TYPE typeIn, long low, long high)
{
	return ty_build_subrange(tr_get_type("Integer"), low, high);
}

TYPE tr_create_array_type(TYPE typeIn, INDEX_LIST indexIn)
{
	//check work
	INDEX_LIST curIndex = indexIn;
	INDEX_LIST index = indexIn;
	long low,high;

	while(1)
	{
		if(curIndex == NULL)
			break;
		if( ty_query(curIndex->type) == TYSUBRANGE )
		{
			ty_query_subrange(curIndex->type, &low, &high);
			if( low > high )
			{
				error("Empty subrange in array index" );
				index = NULL;
				break;
			}
		}
		curIndex = curIndex->next;
	}
	if( typeIn != NULL)
	{
		if( check_data_type(typeIn) == FALSE)
		{
			error("Data type expected for array elements" );
			return ty_build_basic(TYERROR);
		}
		if( index == NULL )
			error("Illegal index type (ignored)" );
		return ty_build_array(typeIn, index);
	}
	else
	{
		error("Data type expected for array elements" );
		return NULL;
	}
}

int tr_type_resolve()
{
	TYPE curtype = ty_get_unresolved();
	TYPE nexttype;
	TYPE object_type;
	ST_ID bucket_rec;
	int ret;
	while(curtype != NULL)
	{
		ty_query_ptr(curtype, &bucket_rec, &nexttype);
		object_type = get_type_by_id(bucket_rec);

		if( object_type != NULL)
		{
			ret = ty_resolve_ptr(curtype, object_type);
		}
		else
		{
				error("Unresolved type name: \"%s\"", st_get_id_str(bucket_rec));
		}
		curtype = nexttype;
	}
	return 0;
}

//add the exist para list to another one
PARAM_LIST tr_add_para(PARAM_LIST paraDes, PARAM_LIST paraIn)
{
	PARAM_LIST curPara;
	PARAM_LIST dupe_check;
	
	if(paraIn != NULL && paraIn != NULL)
	{
		curPara = paraDes;
		while(curPara->next != NULL)
		{
			dupe_check = paraIn;
			//check if there is duplacate of the parameter name or not
			while(1)
			{
				if(dupe_check == NULL)
					break;
				if(strcmp(st_get_id_str(dupe_check->id), 
						st_get_id_str(curPara->id)) == 0 )
				{
					error("Duplicate parameter name: \"%s\"", st_get_id_str(curPara->id));
				}
				dupe_check = dupe_check->next;
			}

			curPara = curPara->next;
		}
		curPara->next = paraIn;
		paraIn->prev = curPara;
		return paraDes;
	}
	return NULL;
}

PARAM_LIST tr_create_para(ID_LIST idlistIn, char *typenameIn,  BOOLEAN is_ref)
{
	ID_LIST curidlist;
	PARAM_LIST curparalist;
	PARAM_LIST outparalist = NULL;
	
	int i;
	
	curidlist = idlistIn;	
	if(idlistIn != NULL)
	{
		if(typenameIn != NULL)
		{
			
			
			curidlist = idlistIn;
			
			while(1)
			{
				if(curidlist == NULL)
					break;
				curparalist = (PARAM_LIST)malloc(sizeof(PARAM));
				curparalist->id= st_enter_id(curidlist->id);
				curparalist->type = tr_get_type(typenameIn);
				if(check_simple_type(curparalist->type) == FALSE)
					error("Parameter type must be a simple type");
				curparalist->sc = NO_SC;
				curparalist->err = TRUE;
				curparalist->is_ref = is_ref;
				if(outparalist == NULL)
				{
					curparalist->next = NULL;
					curparalist->prev = NULL;
				}
				else
				{
					outparalist->prev = curparalist;
					curparalist->next = outparalist;
				}
				outparalist = curparalist;
				curidlist = curidlist->next;
			}
			return outparalist;
			
		}
		else
		{
			// to be added 
		}
	}
	return NULL;
}


INDEX_LIST tr_add_index(INDEX_LIST indexDes, TYPE typeIn)
{
	INDEX_LIST newIndex;
	INDEX_LIST curIndex = indexDes;
	if(indexDes != NULL && typeIn != NULL)
	{
		newIndex = tr_create_index(typeIn);
		// add the typeIn to the end of the index list
		while(curIndex->next != NULL)
			curIndex = curIndex->next;
		curIndex->next = newIndex;
		newIndex->prev = curIndex;
		return indexDes;
	}
	else
	{
		error("type or index in is NULL");
		return NULL;
	}
}

INDEX_LIST tr_create_index(TYPE typeIn)
{
	INDEX_LIST retIndex;
	if( typeIn != NULL )
	{
		retIndex = (INDEX_LIST)malloc(sizeof(INDEX));
		retIndex->type = typeIn;
		retIndex->next = NULL;
		retIndex->prev = NULL;
		return retIndex;
	}
	else
	{
		error("type in is NULL");
		return NULL;
	}
}

ID_LIST tr_add_idlist(ID_LIST idlistDes, char *idIn)
{
	ID_LIST newList;
	if(idlistDes != NULL && idIn != NULL)
	{
		newList = tr_create_idlist(idIn);
		newList->next = idlistDes;
		idlistDes->prev = newList;
		return newList;
	}
	else
	{
		error("id list or id name in is NULL");
		return NULL;
	}
}

ID_LIST tr_create_idlist(char *idIn)
{
	ID_LIST idList;
	if( idIn != NULL )
	{
		idList = (ID_LIST)malloc(sizeof(ID_T));
		idList->id = idIn;
		idList->next = NULL;
		idList->prev = NULL;
		return idList;
	}
	else
	{
		error("id name in is NULL");
		return NULL;
	}
}

/*install a list of ID to hash table & stack*/
BOOLEAN tr_install_idlist( ID_LIST idlistIn, TYPE typeIn)
{
	int ret;
	char *id;
	ST_DR dr;
	ID_LIST curList = idlistIn;
	BOOLEAN outcome = TRUE;
	BOOLEAN valid = TRUE;
	if( idlistIn != NULL && typeIn != NULL)
	{
		if(check_data_type(typeIn) == FALSE)
		{
			valid = FALSE;
			error("Variable(s) must be of data type");
		}
			 
		while(1)
		{
			id = curList->id;
			dr = stdr_alloc();
			dr->tag = GDECL;
			dr->u.decl.type = typeIn;
			dr->u.decl.sc = NO_SC;
			if(ty_query(typeIn) == TYERROR)
				dr->u.decl.err = TRUE;
			else
				dr->u.decl.err = FALSE;	
			ret = st_install(st_enter_id(id),dr);
			if( ret == FALSE )
			{
				error( "Duplicate variable declaration: \"%s\"", id );
				stdr_free(dr);
				curList->valid = FALSE;
				outcome = FALSE;
			}
			else
			{
				curList->valid = TRUE;
			}
			
			if(valid == FALSE)
				curList->valid = FALSE;
				
			if( curList->next != NULL )
			{
				curList = curList->next;
			}
			else
			{
				break;
			}
		}
	}
	else
	{
		error("idlistIn or typeIn name in is NULL");
	}
	return outcome;
}

FUNC_HEAD tr_install_func(char* funcname , TYPE typeIn)
{
	int ret;
	ST_DR dr;
	int block;
	
	FUNC_HEAD funchead = (FUNC_HEAD)malloc(sizeof(PTR_OBJ));
	
	dr = stdr_alloc();
	dr->tag = FDECL;
	dr->u.decl.type = typeIn;
	dr->u.decl.v.global_func_name = funcname;
	dr->u.decl.sc = NO_SC;
//	dr->u.decl.sc = 2;
//printf("install >>>%s\n", funcname);
//	printf("############### sc:%d\n", dr->u.decl.sc);
	if(ty_query(typeIn) == TYERROR)
		dr->u.decl.err = TRUE;
	else
		dr->u.decl.err = FALSE;
	funchead->id = 	st_enter_id(funcname);
	funchead->type = typeIn;
	funchead->funname = funcname;
	ret = st_install(funchead->id,dr);
	if( ret == FALSE)
	{
		dr = st_lookup(st_lookup_id(funcname), &block);
		dr->u.decl.err = TRUE;
	}
//	printf("<<<<<<<<<<<<<<<< err:%d\n", dr->u.decl.err);
	return funchead;
}

BOOLEAN tr_set_func_sc(FUNC_HEAD funchead, STORAGE_CLASS sc_in)
{
	ST_ID bucket_rec;
	ST_DR stack_rec;
	int block;
	if( funchead != NULL )
	{
//		printf("set >>>>%s\n", funchead->funname);
		bucket_rec = funchead->id;
		stack_rec = st_lookup(bucket_rec, &block);
		if( stack_rec != NULL )
		{
//			printf("1############## sc:%d\n", stack_rec->u.decl.sc);
			stack_rec->u.decl.sc = sc_in;
//						printf("############### find the func\n");
			
//			printf(">>>>>>>>>>>>> err%d\n", stack_rec->u.decl.err);
			if(stack_rec->u.decl.err == TRUE 
				&& (sc_in == EXTERN_SC || sc_in == STATIC_SC) )
			{
				ty_error_report(DUP_FUNC_EX_FW, NULL);
			}
		}
		else
		{
//			printf("###############can't find the func\n");
		}	
	}
	else
	{
//		printf("###############can't find the funcname\n");
	}	

	
	return TRUE;
}
// add program name to hash
int tr_add_program(char *program)
{
	st_enter_id(program);
}

EXPR tr_make_intval_node(long intval)
{
	EXPR ptree = (EXPR)malloc(sizeof(EXPR_NODE));
	ptree->tag = INTCONST;
	ptree->type = ty_build_basic(TYSIGNEDLONGINT);
	ptree->u.intval = intval;
	return ptree;
}

EXPR tr_make_realval_node(double realval)
{
	EXPR ptree = (EXPR)malloc(sizeof(EXPR_NODE));
	ptree->tag = REALCONST;
	ptree->type = ty_build_basic(TYDOUBLE);
	ptree->u.realval = realval;
	return ptree;
}

EXPR tr_make_strval_node(char* strval)
{
	EXPR ptree = (EXPR)malloc(sizeof(EXPR_NODE));
	ptree->tag = STRCONST;
	ptree->type = ty_build_basic(TYVOID);
	ptree->u.strval = strval;
	return ptree;
}

EXPR tr_make_gid_node(char *gid_name)
{
//	printf("eeeeeeeeeeeeeeeeeeeeewgid name:%s\n", gid_name);
//	fflush(stdout);
//	printf("%d\n", ty_query(tr_get_type(gid_name)));
	ST_ID gid = st_lookup_id(gid_name);
	if(gid == NULL)
	{
		ty_error_report(UNDEC_ID, gid_name );
		return NULL;
	}
	EXPR ptree = (EXPR)malloc(sizeof(EXPR_NODE));
	int block;
	PARAM_LIST params;
	BOOLEAN check_args;
	ST_DR stack_rec;
	stack_rec = st_lookup(gid, &block);
	
	
	ptree->u.gid = gid;
	if( ty_query( tr_get_type(gid_name)) == TYFUNC)
	{
		ptree->tag = FCALL;
//		error("eeeeeeeeeeeeeeeeeeeeewgid name:%s\n", gid_name);	
		ptree->type = ty_query_func(tr_get_type(gid_name), &params, &check_args);
		// prinf and scanf are special, they don't have fixed length argument list
		if(params == NULL && strncasecmp (gid_name, "printf") != 0 &&  strncasecmp (gid_name, "scanf") != 0)
		{
			gen_process_actual_para_list(NULL);
//			printf("eeeeeeeeeeeeeeeeeeeeewgid name:%s\n", gid_name);	
		}
	}
	else
	{
		ptree->tag = GID;
		ptree->type = get_type_by_id(gid);
	}
	ptree->name = gid_name;
//		printf("????????%d\n", alloc_arglist_or_not);
	return ptree;
}
EXPR tr_make_lvar_node(BOOLEAN is_ref, int offset, int link_count)
{
	return NULL;
}

EXPR tr_make_lfun_node(char * global_name, int link_count)
{
	return NULL;
}

EXPR tr_make_nullop_node(EXPR_NULLOP op)
{
	return NULL;
}

EXPR tr_make_unop_node(EXPR_UNOP op, EXPR node)
{
	EXPR ptree = (EXPR)malloc(sizeof(EXPR_NODE));
	ptree->tag = UNOP;
	ptree->type = node->type;
//	printf("&&&&&&&&&&&");
	ptree->u.unop.op = op;
	ptree->u.unop.operand = node;
//		printf("????????in EXPR tr_make_unop_node(char *gid_name)\n");
	return ptree;
}

EXPR tr_make_binop_node(EXPR_BINOP op, EXPR leftnode, EXPR rightnode)
{
	EXPR ptree = (EXPR)malloc(sizeof(EXPR_NODE));
	ptree->tag = BINOP;
//	printf("!!!!!!!!!!%d\n", ty_query(leftnode->type));
//	fflush(stderr);
//	printf("!!!!!!!!!!%d\n", ty_query(rightnode->type));
//	fflush(stderr);
	ptree->type = type_judege(leftnode->type, rightnode->type) ;
//	printf(">>>>>>>>>>%d\n", ty_query(ptree->type));
	fflush(stderr);
	ptree->u.binop.op = op;
	ptree->u.binop.left = leftnode;
	ptree->u.binop.right = rightnode;
	return ptree;
}

EXPR tr_make_fcall_node(EXPR_LIST args, EXPR node)
{
	return NULL;
}


long tr_eval_const_int(EXPR exprIn)
{
	long outcome;
	EXPR ptree = exprIn;
	if(exprIn->tag == UNOP)
	{
		ptree = ptree->u.unop.operand;
		if( ptree->u.unop.op == NEG_OP )
		{
			return -(ptree->u.intval);
		}
		else
		{
			return ptree->u.intval;
		}
			
	}
	else
	{
		outcome = ptree->u.intval;
	}
	return outcome;
}

P_EXPR_ID tr_make_expr_id(EXPR exprIn, char* id_name, FUNC_HEAD cur_func_head)
{
	P_EXPR_ID p_expr_id = (P_EXPR_ID)malloc(sizeof(EXPR_ID));
	p_expr_id->expr = NULL;
	
	
	int block;
	ST_DR stack_rec;
	TYPE type_func;
	PARAM_LIST para_func;
	BOOLEAN check_args;
	if( id_name != NULL )
	{
    	p_expr_id->id = st_lookup_id(id_name);
		if( cur_func_head != NULL )
		{
    		if(cur_func_head->id != p_expr_id->id)
        		p_expr_id->expr = tr_make_gid_node(id_name);
        	else
        		p_expr_id->expr = NULL;
        }
        else
        {
        	p_expr_id->expr = tr_make_gid_node(id_name);
        }
    	stack_rec = st_lookup(p_expr_id->id, &block);
		if( stack_rec == NULL )
			return NULL;
		if( stack_rec->tag != FDECL )
		{
			b_push_ext_addr(id_name);
		}
	}
	else if(exprIn != NULL)
	{
		p_expr_id->expr = exprIn;
		p_expr_id->id = NULL;
	}
	return p_expr_id;
}


#define FUNC_HEAD_STACK_DEPTH 20
static FUNC_HEAD func_head_stack[FUNC_HEAD_STACK_DEPTH];
static int func_head_stack_sp = 0;
void tr_funchead_push(FUNC_HEAD head_in)
{
//	error("~~~~~~~~~~~~~~push\n");
	fflush(stdout);
	if( func_head_stack_sp < FUNC_HEAD_STACK_DEPTH-1)
		func_head_stack[func_head_stack_sp++] = head_in;
	else
		error("func stack overflow\n");
	return;
}
FUNC_HEAD tr_funchead_peep()
{
//	printf("~~~~~~~~~~~~~~peep\n");
	fflush(stdout);
	if( func_head_stack_sp > 0 )
		return func_head_stack[func_head_stack_sp-1];
	else
	{
		error("func stack underflow\n");
		return NULL;
	}	
}
FUNC_HEAD tr_funchead_pop()
{
//	error("~~~~~~~~~~~~~~pop\n");
	fflush(stdout);
	if( func_head_stack_sp > 0 )
		return func_head_stack[--func_head_stack_sp];
	else
	{
		error("func stack underflow\n");
		return NULL;
	}	
}

EXPR_LIST tr_make_exprlist(EXPR_LIST exprlistIn, EXPR exprIn)
{
	EXPR_LIST cur_exprlist;
	EXPR_LIST iter_exprlist = exprlistIn;
	
	cur_exprlist = (EXPR_LIST)malloc(sizeof(EXPR_LIST_NODE));
	cur_exprlist->expr = exprIn;
	
	if(exprlistIn == NULL)
	{
		cur_exprlist->next = NULL;
		return cur_exprlist;
	}
	else
	{
		while(iter_exprlist->next != NULL)
		{
			iter_exprlist = iter_exprlist->next;
		}
		iter_exprlist->next = cur_exprlist;
		return exprlistIn;
	}
	
	
}

//
//	CANT_COVERT_NODATA, CANT_SET_RET, DUP_FUNC_EX_FW, EXP_PRCD_NAME, ILLEGAL_CONVERT, 
//	INCOMPAT_ARG_2_COMP_OP, NONNUM_ARG_2_ARITH_OP, UNDEC_ID,  REQ_1_VALUE_LEFT
void ty_error_report(ERROR_TYPE error_type, void *input)
{
	switch (error_type)
    {
    	case CANT_COVERT_NODATA:
    		error("Cannot convert between nondata types");
    	break;
    	case CANT_SET_RET:
    	break;
    	case DUP_FUNC_EX_FW:
    		error("Duplicate forward or external function declaration");
    	break;
    	case EXP_PRCD_NAME:
    		error("Expected procedure name, saw data");
    	break;
    	case ILLEGAL_CONVERT:
    		error("Illegal conversion");
    	break;
    	case INCOMPAT_ARG_2_COMP_OP:
    		error("Incompatible type arguments to comparison operator");
    	break;
    	case NONNUM_ARG_2_ARITH_OP:
    		error("Nonnumerical type argument(s) to arithmetic operation");
    	break;
    	case PROC_EXPECTED:
    		error("Procedure call expected");
    	break;
    	case UNDEC_ID:
    		error("Undeclared identifier \"%s\" in expression", (char*)input);
    	break;
    	case REQ_1_VALUE_LEFT:
    		error("Assignment requires l-value on the left");
    	break;
    	default:
    	break;
    }
	return;
}

BOOLEAN tr_check_proc(EXPR exprIn)
{
	BOOLEAN ret = FALSE;
	if( exprIn != NULL)
	{
//		error(">>>>>>>>>>tag:%d", exprIn->tag);
		if( exprIn->tag == GID)
		{
			ty_error_report(EXP_PRCD_NAME, NULL);
		}
		else if( exprIn->tag == INTCONST || exprIn->tag == REALCONST
			|| exprIn->tag == STRCONST)
		{
			ty_error_report(PROC_EXPECTED, NULL);
		}
		else
		{
			ret = TRUE;
		}
	}
	return ret;
}

BOOLEAN tr_check_assignment(EXPR exprIn)
{
	BOOLEAN ret = FALSE;
	if( exprIn != NULL)
	{
//		error(">>>>>>>>>>tag:%d", exprIn->tag);
		if( exprIn->tag != GID)
		{
			ty_error_report(REQ_1_VALUE_LEFT, NULL);
		}
		else
		{
			ret = TRUE;
		}
	}
	return ret;
}

BOOLEAN tr_check_convert(TYPETAG tag_from, TYPETAG tag_to)
{
	BOOLEAN ret = TRUE;
	if( tag_to == TYVOID)
		ret = FALSE;
	if( tag_from == TYUNSIGNEDLONGINT && tag_to == TYSIGNEDCHAR)
		ret = FALSE;
	if( tag_from == TYSIGNEDLONGINT && tag_to == TYSIGNEDCHAR)
		ret = FALSE;
	if( tag_from == TYUNSIGNEDLONGINT && tag_to == TYUNSIGNEDCHAR)
		ret = FALSE;
	if( tag_from == TYSIGNEDLONGINT && tag_to == TYUNSIGNEDCHAR)
		ret = FALSE;
	if( tag_from == TYDOUBLE && tag_to == TYUNSIGNEDLONGINT)
		ret = FALSE;	
	if( tag_from == TYDOUBLE && tag_to == TYSIGNEDLONGINT)
		ret = FALSE;
	if( tag_from == TYDOUBLE && tag_to == TYSIGNEDCHAR)
		ret = FALSE;
	if( tag_from == TYDOUBLE && tag_to == TYUNSIGNEDCHAR)
		ret = FALSE;
	if( tag_from == TYFLOAT && tag_to == TYSIGNEDCHAR)
		ret = FALSE;
	if( tag_from == TYFLOAT && tag_to == TYUNSIGNEDCHAR)
		ret = FALSE;
	if( tag_from == TYUNSIGNEDCHAR && tag_to == TYSIGNEDLONGINT)
		ret = FALSE;

	return ret;	
}

BOOLEAN tr_check_numeric(TYPETAG tagIn)
{
	BOOLEAN ret = TRUE;
	if(tagIn == TYUNSIGNEDCHAR)
		ret = FALSE;
	if(tagIn == TYSIGNEDCHAR)
		ret = FALSE;	
	return ret;	
}

BOOLEAN tr_check_comp_op(TYPETAG tagIn)
{
	BOOLEAN ret = TRUE;
	if(tagIn == TYUNSIGNEDCHAR)
		ret = FALSE;
	if(tagIn == TYSIGNEDCHAR)
		ret = FALSE;
	return ret;	
}