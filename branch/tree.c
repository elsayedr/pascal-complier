#include "symtab.h"
#include "types.h"
#include "tree.h"
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

PARAM_LIST tr_create_para(ID_LIST idlistIn, char *typenameIn,  BOOLEAN is_ref)
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

ID_LIST tr_add_idlist(ID_LIST idlistDes, ID_LIST idlistIn)
{
	return NULL;
}

ID_LIST tr_create_idlist(char *idIn)
{
	return NULL;
}

/*install a list of ID to hash table*/
int tr_install_idlist(ID_LIST idlistIn, TYPE typeIn)
{
	return 0;
}
