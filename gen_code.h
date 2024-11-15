/* $Id: gen_code.h, 2024/11/15 $ */

#ifndef _GEN_CODE_H
#define _GEN_CODE_H
#include <stdio.h>
#include "ast.h"
#include "bof.h"
#include "instruction.h"
#include "code_seq.h"
#include "code.h"



extern  code_seq gen_code_var_decls(var_decls_t vds);

extern code_seq gen_code_var_decl(var_decl_t vd);

extern code_seq gen_code_idents(ident_list_t ids);

extern code_seq gen_code_ident(ident_t id);

extern code_seq gen_code_stmt(stmt_t stmt);

extern code_seq gen_code_assign_stmt(assign_stmt_t stmt);

extern code_seq gen_code_callStmt(call_stmt_t *stmt);

extern code_seq gen_code_blockStmt(block_stmt_t *block);

extern code_seq gen_code_ifStmt(if_stmt_t *stmt);


extern code_seq gen_code_whileStmt(while_stmt_t *stmt);


extern code_seq gen_code_readStmt(read_stmt_t *stmt);

extern code_seq gen_code_printStmt(print_stmt_t *stmt);

extern code_seq gen_code_expr(expr_t exp);

extern code_seq gen_code_binary_op_expr(binary_op_expr_t exp);


extern code_seq gen_code_op(token_t op);

extern code_seq gen_code_arith_op(token_t arith_op);


extern code_seq gen_code_rel_op(token_t rel_op);


extern code_seq gen_code_ident(ident_t id);


extern code_seq gen_code_number(number_t num);


extern code_seq gen_code_logical_not_expr(expr_t exp);


#endif