#include "stdlib.h"
#include "encode.h"
#include "symtab.h"
#include "types.h"
#include "backend-x86.h"

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

