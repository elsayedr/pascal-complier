#ifndef PPC_PROJ1_TREE_H_
#define PPC_PROJ1_TREE_H_
/*with the type and new type name, we can create and a new type to the hash table
before we create a new ID for hash, we should first check if it already exists
in the hash table, we can check that by  char *id.
*/
int tr_make_type(char *id, TYPE typeIn);

/*locate the type by its name from the hash, if it can't be found
give an error message */
TYPE tr_get_type(char *typenameIn);

/*not used in this assignment*/
TYPE tr_get_const_value(char *idnameIn);

/* create different types*/
TYPE tr_create_pointer_type(TYPE typeIn);
TYPE tr_create_func_type(TYPE typeIn, PARAM_LIST params, BOOLEAN check_args);
TYPE tr_create_subrange_type(TYPE typeIn, long low, long high);
TYPE tr_create_array_type(TYPE typeIn, INDEX_LIST indexIn);

/*create a parameter list, we can create an ID in hash for it.
However we don't know its base type, we will figure that out in tr_edit_para*/
PARAM_LIST tr_create_para(char *varnameIn);

/*add new parameter to the existing list and return the new one
if paraDes == NULL, that means this parameter is the first one */
PARAM_LIST tr_add_para(PARAM_LIST paraDes, PARAM_LIST paraIn);

/*here we have more information about each parameter, 
we can locate the basic type by the name and fulfill the type of the parameter
as well as the is_ref*/
PARAM_LIST tr_edit_para(PARAM_LIST paraIn, char *typenameIn,  BOOLEAN is_ref);

/*add new type to the index list*/
INDEX_LIST tr_add_index(INDEX_LIST indexDes, TYPE typeIn);
/*create a index list according to existing type*/
INDEX_LIST tr_create_index(TYPE typeIn);


#endif /* #ifndef PPC_PROJ1_TREE_H_ */
