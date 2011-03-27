#include "stdlib.h"
#include "symtab.h"
#include "types.h"
#include "tree.h"
#include "message.h"


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
			if( stack_rec->tag != TYPENAME)
				warning("the stack record found is not a type\n");
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
			if( stack_rec->tag != TYPENAME)
				warning("the stack record found is not a type\n");
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
			if( stack_rec->tag != TYPENAME)
				warning("the stack record found is not a type\n");
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


TYPE tr_create_func_type(TYPE typeIn, PARAM_LIST params, BOOLEAN check_args)
{
	TYPE type;
	if(typeIn == NULL)
		type = ty_build_basic(TYVOID);
	else
		type = 	typeIn;
	if(check_simple_type(type) == FALSE && typeIn != NULL)
		error("Function return type must be simple type");
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

// add program name to hash
int tr_add_program(char *program)
{
	st_enter_id(program);
}

