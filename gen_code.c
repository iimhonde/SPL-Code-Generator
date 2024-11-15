/* $Id: gen_code.c, 2024/11/15 $ */

#include <limits.h>
#include <string.h>
#include "spl.tab.h"
#include "ast.h"
#include "code.h"
#include "id_use.h"
#include "literal_table.h"
#include "gen_code.h"
#include "utilities.h"
#include "regname.h"


code_seq gen_code_initialize(){}

void gen_code_program(BOFFILE bf, block_t prog){}

code_seq gen_code_var_decls(var_decls_t vds){}

code_seq gen_code_var_decl(var_decl_t vd){}

code_seq gen_code_idents(ident_list_t ids){}

code_seq gen_code_ident(ident_t id){}

code_seq gen_code_stmt(stmt_t stmt){}

code_seq gen_code_assign_stmt(assign_stmt_t stmt){}

code_seq gen_code_callStmt(call_stmt_t *stmt){}

code_seq gen_code_blockStmt(block_stmt_t *block){}

code_seq gen_code_ifStmt(if_stmt_t *stmt){}


code_seq gen_code_whileStmt(while_stmt_t *stmt){}


code_seq gen_code_readStmt(read_stmt_t *stmt){}

code_seq gen_code_printStmt(print_stmt_t *stmt){}

code_seq gen_code_expr(expr_t exp){}

code_seq gen_code_binary_op_expr(binary_op_expr_t exp){}


code_seq gen_code_op(token_t op){}

code_seq gen_code_arith_op(token_t arith_op){}


code_seq gen_code_rel_op(token_t rel_op){}


code_seq gen_code_ident(ident_t id){}


code_seq gen_code_number(number_t num){}


code_seq gen_code_logical_not_expr(expr_t exp){}