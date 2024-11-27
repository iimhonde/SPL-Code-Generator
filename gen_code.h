#ifndef GEN_CODE_H
#define GEN_CODE_H

#include "code_seq.h"
#include "code_utils.h"
#include "ast.h"
#include "bof.h"

// Initialize the code generator
void gen_code_initialize();

// Output the program to the BOF file
void gen_code_program(BOFFILE bf, block_t *prog);

// Generate code for a block
code_seq gen_code_block(block_t *block);

// Generate code for constant declarations
code_seq gen_code_const_decls(const_decls_t *const_decls);
code_seq gen_code_const_decl(const_decl_t *cd);
code_seq gen_code_const_def_list(const_def_list_t *cdl);
code_seq gen_code_const_def(const_def_t *def);

// Generate code for variable declarations
code_seq gen_code_var_decls(var_decls_t *vds);
code_seq gen_code_var_decl(var_decl_t *vd);
code_seq gen_code_idents(ident_list_t *ids);
code_seq gen_code_ident(ident_t *id);

// Generate code for statements
code_seq gen_code_stmts(stmts_t *stmts);
code_seq gen_code_stmt(stmt_t *stmt);
code_seq gen_code_assignStmt(assign_stmt_t *stmt);
code_seq gen_code_callStmt(call_stmt_t *stmt);
code_seq gen_code_blockStmt(block_stmt_t *block_stmt);
code_seq gen_code_condition(condition_t *cond);
code_seq gen_code_whileStmt(while_stmt_t *stmt);
code_seq gen_code_ifStmt(if_stmt_t *stmt);
code_seq gen_code_readStmt(read_stmt_t *stmt);
code_seq gen_code_printStmt(print_stmt_t *stmt);

// Generate code for expressions
code_seq gen_code_expr(expr_t *exp);
code_seq gen_code_binary_op_expr(binary_op_expr_t *exp);
code_seq gen_code_op(token_t *op);
code_seq gen_code_arith_op(token_t *arith_op);
code_seq gen_code_rel_op(token_t *rel_op);
code_seq gen_code_number(number_t *num);
code_seq gen_code_logical_not_expr(negated_expr_t *exp);

// Helper functions (for BOF output)
static void gen_code_output_seq(BOFFILE bf, code_seq cs);
static BOFHeader gen_code_program_header(code_seq main_cs);
static void gen_code_output_literals(BOFFILE bf);
static void gen_code_output_program(BOFFILE bf, code_seq main_cs);

#endif // GEN_CODE_H
