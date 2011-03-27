#ifndef PPC_PROJ1_TREE_H_
#define PPC_PROJ1_TREE_H_

#include "types.h"

#define MAX_PARAMETER	256

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

/*add a new program to the ID list*/
int tr_add_program(char *program);
#endif /* #ifndef PPC_PROJ1_TREE_H_ */
