#include "symtab.h"
#include "types.h"
#include "message.h"

int tr_make_type(char *id, TYPE typeIn)
{
	return 0;
}

TYPE tr_get_type(char *itm)
{
	return NULL;
}

TYPE tr_create_pointer_type(TYPE typeIn)
{
	return NULL;
}

TYPE tr_create_func_type(TYPE typeIn, PARAM_LIST params, BOOLEAN check_args)
{
	return NULL;
}

TYPE tr_create_subrange_type(TYPE typeIn, long low, long high)
{
	return NULL;
}

TYPE tr_create_array_type(TYPE typeIn, INDEX_LIST indexIn)
{
	return NULL;
}

PARAM_LIST tr_add_para(PARAM_LIST paraDes, PARAM_LIST paraIn)
{
	return NULL;
}

PARAM_LIST tr_create_para(char *varnameIn)
{
	return NULL;
}

PARAM_LIST tr_edit_para(PARAM_LIST paraIn, char *typenameIn,  BOOLEAN is_ref)
{
	return NULL;
}

INDEX_LIST tr_add_index(INDEX_LIST indexDes, TYPE typeIn)
{
	return NULL;
}
INDEX_LIST tr_create_index(TYPE typeIn)
{
	return NULL;
}


