#include <stdio.h>
#include "stdlib.h"
#include "encode.h"
#include "symtab.h"
#include "types.h"
#include "backend-x86.h"
#include "tree.h"

int get_array_size(TYPE type, int *size, int *alignment )
{
	int base_size;
	int base_alignment;
	int index_size;
	int index_alignment;
	int index_total = 1;
	INDEX_LIST index;
	TYPE bastype;

	bastype = ty_query_array(type, &index);
	get_var_size(bastype, &base_size, &base_alignment );
	while(1)
	{
		if( index == NULL)
			break;
		index_total *= get_subrange_size(index->type, &index_size, &index_alignment );
		index = index->next;
	}
	
	*size = index_total * base_size;
	*alignment = base_alignment;
	return 0;
}

int get_subrange_size(TYPE type, int *size, int *alignment )
{
	long low,high;
	ty_query_subrange(type, &low, &high);
	*alignment =4;
	*size = 4;
	return (high-low+1);
}

int get_var_size(TYPE type, int *size, int *alignment )
{
	TYPETAG typetag;
	typetag = ty_query(type);
	switch( typetag ) 
	{
		case TYUNSIGNEDCHAR:
		case TYSIGNEDCHAR:		    
		    *size = 1; *alignment = 1;
		    break;
		    
		case TYSIGNEDSHORTINT:
		case TYUNSIGNEDSHORTINT:
		    *size = 2; *alignment = 2;
		    break;
		        		    
		case TYFLOAT:
		case TYSIGNEDINT:
		case TYUNSIGNEDINT:
		case TYSIGNEDLONGINT:
		case TYUNSIGNEDLONGINT:
		case TYFUNC:
		case TYPTR:
			*size = 4; *alignment = 4;
		    break;
		    
		case TYDOUBLE:
		    *size = 8; *alignment = 8;
		    break;
		    
		case TYLONGDOUBLE:
		    *size = 16; *alignment = 16;
		    break;
		    
		case TYARRAY:
			get_array_size(type, size, alignment );
			break;
			
		case TYSUBRANGE:
			get_subrange_size(type, size, alignment );
		    break;
		    
		default:
			*size = 4; *alignment = 4;
//			TYVOID    
//		    TYSTRUCT
//			TYUNION
//			TYENUM
//			TYSET
//			TYBITFIELD
//			TYSUBRANGE
//			TYERROR
			break;
	}
	return 0;
}


TYPETAG get_expr_type(EXPR exprIn)
{
	return ty_query(exprIn->type);
}

TYPE get_type_id(ST_ID bucket_rec)
{
	ST_DR stack_rec;
	int block;
	//look for record on hash
	//if it does not exit, create one hash record
	//if yes, go for it on the stack
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
//			error("Undeclared type name: \"%s\"", itm);
		}
	}
	return NULL;
}

void gen_var_asm(ID_LIST idlistIn)
{
	int ret;
	char *id;
	ST_DR dr;
	ST_ID bucket_rec;
	ID_LIST curList = idlistIn;
	int alignment;
	int size;
	TYPETAG typetag;
	
	if( idlistIn != NULL)
	{
		while(1)
		{
			id = curList->id;
			if(curList->valid == TRUE)
			{
    			bucket_rec = st_lookup_id(id);
    			if( bucket_rec == NULL )
    			{
    				error( "Can't find bucket record: \"%s\"", id );
    			}
    			dr = st_lookup(bucket_rec, &ret);
    			if( dr == FALSE )
    			{
    				error( "Can't find stack record: \"%s\"", id );
    			}
    
    			get_var_size(dr->u.decl.type, &size, &alignment );
    			b_global_decl (id, alignment, size);
    			b_skip(size);
			}
			
			if( curList->next != NULL )
			{
				curList = curList->next;
				free(curList->prev);
			}
			else
			{
				free(curList);
				break;
			}
		}
	}
	else
	{
		error("idlistIn in is NULL\n");
	}
}

void gen_tree_asm(EXPR exprIn)
{
//	printf(">>>>>>>>type:%d\n", exprIn->tag);
	TYPETAG from_type;
	TYPETAG to_type;
	TYPETAG left_type;
	TYPETAG right_type;
	TYPETAG temp_type_tag;
	TYPE temp_type;
	int block;
	ST_DR stack_rec;
	ST_ID bucket_rec;
	BOOLEAN arithm_op_vialate = FALSE;
	BOOLEAN comp_op_vialate = FALSE;
//	printf("ok!!!!!!!!!!!!!!!!!!!!!!!\n");
//	printf("%d!\n", exprIn->tag);
	if(exprIn == NULL)
		return;
		
	if( exprIn->tag == INTCONST )
	{
		b_push_const_int(exprIn->u.intval);
	}
	else if( exprIn->tag == REALCONST)
	{
		b_push_const_double(exprIn->u.realval);
	}
	else if( exprIn->tag == STRCONST)
	{
		b_push_const_string(exprIn->u.strval);
	}
	else if(exprIn->tag == GID || exprIn->tag == FCALL)
	{
		stack_rec = st_lookup(exprIn->u.gid, &block);
		if( stack_rec->tag == FDECL )
		{
			b_funcall_by_name (exprIn->name, ty_query(exprIn->type));
		}
		else
		{
			b_push_ext_addr(exprIn->name);
//			if( get_expr_type(exprIn) != TYPTR)
				b_deref(get_expr_type(exprIn));
			if( get_expr_type(exprIn) == TYFLOAT )
			{
				b_convert (TYFLOAT, TYDOUBLE);
				exprIn->type = ty_build_basic(TYDOUBLE);
			}
			else if( get_expr_type(exprIn) == TYUNSIGNEDCHAR )
			{
//				printf("start convert");
//				fflush(stdout);
				b_convert (TYUNSIGNEDCHAR, TYSIGNEDLONGINT);
				exprIn->type = ty_build_basic(TYSIGNEDLONGINT);
//				b_convert (TYUNSIGNEDCHAR, TYUNSIGNEDLONGINT);
//				exprIn->type = ty_build_basic(TYUNSIGNEDLONGINT);
			}
		}
	}
	else if(exprIn->tag == UNOP)
	{
		switch( exprIn->u.unop.op ) 
		{
			case DISPOSE_OP:
				b_alloc_arglist(4);
				gen_tree_asm(exprIn->u.unop.operand);
				b_load_arg (TYPTR);
				b_funcall_by_name ("free", TYVOID);
				break;
				
			case DEREF_OP:
				gen_tree_asm(exprIn->u.unop.operand);
				b_deref (ty_query(ty_query_ptr(exprIn->u.unop.operand->type, &bucket_rec, &temp_type)));
				break;
			case NEW_OP:
				b_push_ext_addr(exprIn->u.unop.operand->name);
//				b_push_ext_addr(exprIn->u.unop.operand->name);
				b_alloc_arglist(4);
				temp_type_tag = ty_query(ty_query_ptr(exprIn->type, &bucket_rec, &temp_type));
				if( temp_type_tag == TYDOUBLE )
					b_push_const_int(8);
				else	
					b_push_const_int(4);
//				b_load_arg (ty_query(ty_query_ptr(exprIn->type, &bucket_rec, &temp_type)));
				b_load_arg (TYUNSIGNEDINT);
				b_funcall_by_name ("malloc", TYPTR);
				b_assign(TYPTR);
				b_pop();
				break;
			case CHR_OP:
				gen_tree_asm(exprIn->u.unop.operand);
				from_type = get_expr_type(exprIn->u.unop.operand);
				if( from_type != TYUNSIGNEDCHAR)
				{
    				if(!tr_check_convert(from_type,TYUNSIGNEDCHAR ))
    				{
    					ty_error_report(ILLEGAL_CONVERT, NULL);
    				}
					b_convert (from_type, TYUNSIGNEDCHAR);
					exprIn->type = ty_build_basic(TYUNSIGNEDCHAR);
				}
				break;
			case ORD_OP:
				gen_tree_asm(exprIn->u.unop.operand);
//				printf("aaaaaaaaaaaaaaaaaaaaaaaa\n");
				from_type = get_expr_type(exprIn->u.unop.operand);
				if( from_type != TYSIGNEDLONGINT)
				{
    				if(!tr_check_convert(from_type,TYSIGNEDLONGINT ))
    				{
    					ty_error_report(ILLEGAL_CONVERT, NULL);
    				}
					b_convert (from_type, TYSIGNEDLONGINT);
					exprIn->type = ty_build_basic(TYSIGNEDLONGINT);
				}
				exprIn->type = exprIn->u.unop.operand->type;
//				printf("aaaaaaaaaaaaaaaaaaaaaaaa\n");
				break;
			case UN_SUCC_OP:
				gen_tree_asm(exprIn->u.unop.operand);

				from_type = get_expr_type(exprIn->u.unop.operand);
//				printf("aaaaaaaaaaaaaaaaaaaaaaaa\n");
				fflush(stdout);
				if( from_type != TYSIGNEDLONGINT)
				{
					if(!tr_check_convert(from_type,TYSIGNEDLONGINT ))
    				{
    					ty_error_report(ILLEGAL_CONVERT, NULL);
    				}
					b_convert (from_type, TYSIGNEDLONGINT);
					exprIn->type = ty_build_basic(TYSIGNEDLONGINT);
				}
				b_push_const_int (1);
				b_arith_rel_op (B_ADD, ty_query(exprIn->u.unop.operand->type));
				exprIn->type = exprIn->u.unop.operand->type;
//				printf("!!!!!!!!!!!!!!!%d\n", get_expr_type(exprIn));
				break;
			case UN_PRED_OP:
				gen_tree_asm(exprIn->u.unop.operand);
				from_type = get_expr_type(exprIn->u.unop.operand);
				if( from_type != TYSIGNEDLONGINT)
				{
					if(!tr_check_convert(from_type,TYSIGNEDLONGINT ))
    				{
    					ty_error_report(ILLEGAL_CONVERT, NULL);
    				}
					b_convert (from_type, TYSIGNEDLONGINT);
					exprIn->type = ty_build_basic(TYSIGNEDLONGINT);
				}
				b_push_const_int (-1);
				b_arith_rel_op (B_ADD, ty_query(exprIn->type));
				break;
			case NEG_OP:
				gen_tree_asm(exprIn->u.unop.operand);
				b_negate(get_expr_type(exprIn->u.unop.operand));
				break;
			default:
				break;
		}
	}
//	    ADD_OP, SUB_OP, MUL_OP, DIV_OP, MOD_OP, REALDIV_OP, EQ_OP, LESS_OP, LE_OP,
//    NE_OP, GE_OP, GREATER_OP, SYMDIFF_OP, OR_OP, XOR_OP, AND_OP, BIN_SUCC_OP,
	else if( exprIn->tag == BINOP )
	{
		arithm_op_vialate = tr_check_numeric(get_expr_type(exprIn->u.binop.left));
		arithm_op_vialate &= tr_check_numeric(get_expr_type(exprIn->u.binop.right));

//		error("%d:%d", get_expr_type(exprIn->u.binop.left), get_expr_type(exprIn->u.binop.right));
		if( exprIn->u.binop.left->tag != INTCONST)
		{
			gen_tree_asm(exprIn->u.binop.left);
			temp_type = type_judege(exprIn->u.binop.left->type, exprIn->type);
			exprIn->type = temp_type;
			if( get_expr_type(exprIn->u.binop.left) != get_expr_type(exprIn))
			{
//				printf("aaaaaaaaaaaaaaaaaaaaaaaa\n");
				if(!tr_check_convert(get_expr_type(exprIn->u.binop.left),get_expr_type(exprIn) ))
				{
					ty_error_report(ILLEGAL_CONVERT, NULL);
				}
				b_convert (get_expr_type(exprIn->u.binop.left), get_expr_type(exprIn));
			}
		}
		else
		{
			temp_type = type_judege(exprIn->u.binop.left->type, exprIn->type);
			exprIn->type = temp_type;
			exprIn->u.binop.left->type = temp_type;
			if(ty_query(temp_type) == TYDOUBLE)
			{
				exprIn->u.binop.left->tag = REALCONST;
				exprIn->u.binop.left->u.realval = exprIn->u.binop.left->u.intval;
			}
			gen_tree_asm(exprIn->u.binop.left);
		}
		
		if( exprIn->u.binop.right->tag != INTCONST)
		{
    		gen_tree_asm(exprIn->u.binop.right);
    		temp_type = type_judege(exprIn->u.binop.left->type, exprIn->type);
    		exprIn->type = temp_type;
    		if( get_expr_type(exprIn->u.binop.right) != get_expr_type(exprIn))
    		{
    			if(!tr_check_convert(get_expr_type(exprIn->u.binop.right),get_expr_type(exprIn) ))
				{
					ty_error_report(ILLEGAL_CONVERT, NULL);
				}
    			b_convert (get_expr_type(exprIn->u.binop.right), get_expr_type(exprIn));
    		}
    	}
    	else
    	{
    		temp_type = type_judege(exprIn->u.binop.right->type, exprIn->type);
			exprIn->type = temp_type;
			exprIn->u.binop.right->type = temp_type;
			if(ty_query(temp_type) == TYDOUBLE)
			{
				exprIn->u.binop.right->tag = REALCONST;
				exprIn->u.binop.right->u.realval = exprIn->u.binop.right->u.intval;
			}
			gen_tree_asm(exprIn->u.binop.right);
    	}
		comp_op_vialate = tr_check_comp_op(get_expr_type(exprIn->u.binop.left));
		comp_op_vialate &= tr_check_comp_op(get_expr_type(exprIn->u.binop.right));
		switch( exprIn->u.binop.op ) 
		{
			case ADD_OP:
				if(!arithm_op_vialate)
					ty_error_report(NONNUM_ARG_2_ARITH_OP, NULL);
    			b_arith_rel_op (B_ADD, ty_query(exprIn->type));
    			break;
			case SUB_OP:
				if(!arithm_op_vialate)
					ty_error_report(NONNUM_ARG_2_ARITH_OP, NULL);
    			b_arith_rel_op (B_SUB, ty_query(exprIn->type));
    			break;
    		case MUL_OP:
    			if(!arithm_op_vialate)
					ty_error_report(NONNUM_ARG_2_ARITH_OP, NULL);
    			b_arith_rel_op (B_MULT, ty_query(exprIn->type));
    			break;
    		case DIV_OP:
    			if(!arithm_op_vialate)
					ty_error_report(NONNUM_ARG_2_ARITH_OP, NULL);
    			b_arith_rel_op (B_DIV, ty_query(exprIn->type));
    			break;
    		case MOD_OP:
    			if(!arithm_op_vialate)
					ty_error_report(NONNUM_ARG_2_ARITH_OP, NULL);
    			b_arith_rel_op (B_MOD, ty_query(exprIn->type));
    			break;
    			  			
    		case EQ_OP:
    			if(!comp_op_vialate)
    				ty_error_report(INCOMPAT_ARG_2_COMP_OP, NULL);
    			b_arith_rel_op (B_EQ, TYSIGNEDLONGINT);
				if(!tr_check_convert(ty_query(exprIn->type),TYSIGNEDCHAR ))
				{
					ty_error_report(ILLEGAL_CONVERT, NULL);
				}
    			b_convert (ty_query(exprIn->type), TYSIGNEDCHAR);
    			exprIn->type = ty_build_basic(TYSIGNEDCHAR);
    			break;
			case LESS_OP:
    			if(!comp_op_vialate)
    				ty_error_report(INCOMPAT_ARG_2_COMP_OP, NULL);
    			b_arith_rel_op (B_LT, TYSIGNEDLONGINT);
    			if(!tr_check_convert(ty_query(exprIn->type),TYSIGNEDCHAR ))
				{
					ty_error_report(ILLEGAL_CONVERT, NULL);
				}
    			b_convert (ty_query(exprIn->type), TYSIGNEDCHAR);
    			exprIn->type = ty_build_basic(TYSIGNEDCHAR);
    			break;
    		case LE_OP:
    			if(!comp_op_vialate)
    				ty_error_report(INCOMPAT_ARG_2_COMP_OP, NULL);
    			b_arith_rel_op (B_LE, TYSIGNEDLONGINT);
    			if(!tr_check_convert(ty_query(exprIn->type),TYSIGNEDCHAR ))
				{
					ty_error_report(ILLEGAL_CONVERT, NULL);
				}
    			b_convert (ty_query(exprIn->type), TYSIGNEDCHAR);
    			exprIn->type = ty_build_basic(TYSIGNEDCHAR);
    			break;
    		case GREATER_OP:
    			if(!comp_op_vialate)
    				ty_error_report(INCOMPAT_ARG_2_COMP_OP, NULL);
    			b_arith_rel_op (B_GT, TYSIGNEDLONGINT);
    			if(!tr_check_convert(ty_query(exprIn->type),TYSIGNEDCHAR ))
				{
					ty_error_report(ILLEGAL_CONVERT, NULL);
				}
    			b_convert (ty_query(exprIn->type), TYSIGNEDCHAR);
    			exprIn->type = ty_build_basic(TYSIGNEDCHAR);
    			break;
    		case GE_OP:
    			if(!comp_op_vialate)
    				ty_error_report(INCOMPAT_ARG_2_COMP_OP, NULL);
    			b_arith_rel_op (B_GE, TYSIGNEDLONGINT);
    			if(!tr_check_convert(ty_query(exprIn->type),TYSIGNEDCHAR ))
				{
					ty_error_report(ILLEGAL_CONVERT, NULL);
				}
    			b_convert (ty_query(exprIn->type), TYSIGNEDCHAR);
    			exprIn->type = ty_build_basic(TYSIGNEDCHAR);
    			break;
    		case NE_OP:
    			if(!comp_op_vialate)
    				ty_error_report(INCOMPAT_ARG_2_COMP_OP, NULL);
    			b_arith_rel_op (B_NE, TYSIGNEDLONGINT);
    			if(!tr_check_convert(ty_query(exprIn->type),TYSIGNEDCHAR ))
				{
					ty_error_report(ILLEGAL_CONVERT, NULL);
				}
    			b_convert (ty_query(exprIn->type), TYSIGNEDCHAR);
    			exprIn->type = ty_build_basic(TYSIGNEDCHAR);
    			break;
	
			default:
			break;
		}
	}
	else
	{
		
	}	
}


EXPR gen_do_assignment(P_EXPR_ID p_expr_id, EXPR expr_in, FUNC_HEAD cur_func_head)
{
//	printf(">>>>>>>>>>>>do assignment\n");
//	fflush(stdout);
	TYPE type_expr_id;
	if( p_expr_id == NULL )
		return NULL;
	ST_ID gid = p_expr_id->id;
	type_expr_id = get_type_id(gid);
	TYPETAG to_type = ty_query(type_expr_id);
	TYPETAG from_type = get_expr_type(expr_in);
	
	PARAM_LIST params;
	BOOLEAN check_args;
	TYPETAG typefunc;
	
	if(cur_func_head != NULL)
	{ 
		typefunc = ty_query(ty_query_func(get_type_id(cur_func_head->id), &params, &check_args));
	}
	else
	{
		//func head should not be nothing
		typefunc = TYVOID;
	}

	//if the gid == current function name
	//it need return value or just do the assignment
	if( gid == cur_func_head->id)
	{
		b_set_return (typefunc);
	}
	else
	{
    	if( to_type != from_type)
    	{
    		if( to_type != TYVOID && from_type != TYVOID 
    			&& to_type != TYFUNC && from_type != TYFUNC
    			&& to_type != TYPTR && from_type != TYPTR)
    		{
    			if(!tr_check_convert(from_type, to_type))
				{
					ty_error_report(ILLEGAL_CONVERT, NULL);
				}
    			b_convert (from_type, to_type);
    		}
    		else
    		{
    			ty_error_report(CANT_COVERT_NODATA, NULL);
    		}
    	}
    	b_assign(to_type);
    	b_pop();
    }
}

void gen_process_local_vars(void)
{
	b_alloc_local_vars (0);
	return;
}

void gen_process_func_return(void)
{
	b_alloc_local_vars (0);
	return;
}

void gen_process_func_head( FUNC_HEAD funchead)
{
	TYPE typefunc;
	PARAM_LIST params;
	BOOLEAN check_args;
	typefunc = ty_query_func(get_type_id(funchead->id), &params, &check_args);
	b_init_formal_param_offset ();
  	b_func_prologue (funchead->funname);
	if(ty_query(typefunc) != TYVOID)
	{
		b_alloc_return_value ( );
//		b_alloc_local_vars ( 8 );
	}
}

void gen_process_prgm_head( FUNC_HEAD funchead)
{
	TYPE typefunc;
	PARAM_LIST params;
	BOOLEAN check_args;
	typefunc = ty_query_func(get_type_id(funchead->id), &params, &check_args);
  	b_func_prologue ("main");
	if(ty_query(typefunc) != TYVOID)
	{
		b_alloc_return_value ( );
//		b_alloc_local_vars ( 8 );
	}
}

void gen_process_func_tail( FUNC_HEAD funchead)
{
	PARAM_LIST params;
	BOOLEAN check_args;
	TYPETAG typefunc_tag = ty_query(ty_query_func(get_type_id(funchead->id), &params, &check_args));
	if( typefunc_tag != TYVOID)
		b_prepare_return (typefunc_tag);
	b_func_epilogue (funchead->funname);
}

void gen_process_prgm_tail( FUNC_HEAD funchead)
{
	PARAM_LIST params;
	BOOLEAN check_args;
	TYPETAG typefunc_tag = ty_query(ty_query_func(get_type_id(funchead->id), &params, &check_args));
	if( typefunc_tag != TYVOID)
		b_prepare_return (typefunc_tag);
	b_func_epilogue ("main");
}

void gen_process_actual_para_list(EXPR_LIST exprlistIn)
{
	int list_len = 0;
	EXPR_LIST cur_exprlist = exprlistIn;
	while(cur_exprlist != NULL)
	{
		cur_exprlist = cur_exprlist->next;
		list_len += 4;
	}
//	printf("????????????????????\n");
	b_alloc_arglist (list_len);

	cur_exprlist = exprlistIn;
	while(cur_exprlist != NULL)
	{
		
		gen_tree_asm(cur_exprlist->expr);
		if( ty_query(cur_exprlist->expr->type) == TYVOID)
		{
			b_load_arg (TYPTR);
		}
		else
		{
			b_load_arg (ty_query(cur_exprlist->expr->type));
		}
		cur_exprlist = cur_exprlist->next;
	}
	return;
}

