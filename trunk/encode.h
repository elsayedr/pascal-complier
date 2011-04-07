#ifndef PPC_PROJ1_ENCODE_H_
#define PPC_PROJ1_ENCODE_H_
#include "tree.h"

void gen_var_asm(ID_LIST idlistIn);
void gen_tree_asm(EXPR exprIn);
EXPR gen_do_assignment(P_EXPR_ID p_expr_id, EXPR expr_in, FUNC_HEAD cur_func_head);
void gen_process_func_head( FUNC_HEAD funchead);
void gen_process_func_tail( FUNC_HEAD funchead);
void gen_process_prgm_head( FUNC_HEAD funchead);
void gen_process_prgm_tail( FUNC_HEAD funchead);
void gen_process_local_vars(void);
void gen_process_actual_para_list(EXPR_LIST exprlistIn);
void gen_process_func_return(void);
#endif /* #ifndef PPC_PROJ1_ENCODE_H_ */

