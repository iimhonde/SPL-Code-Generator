#ifndef GEN_CODE_H
#define GEN_CODE_H

#include "code_seq.h"
#include "code_utils.h"
#include "ast.h"
#include "bof.h"

// Maximum stack size for BOF program
#define MAX_STACK 4096

// Initialize the code generator
void gen_code_initialize(void);

// Generate code for the program into a BOFFILE
void gen_code_program(BOFFILE bf, block_t prog);

// Generate code for a block
code_seq gen_code_block(block_t block);

// Generate code for constant declarations
code_seq gen_code_const_decls(const_decls_t const_decls);

// Generate code for a single constant declaration
code_seq gen_code_const_decl(const_decl_t cd);

// Generate code for a list of constant definitions
code_seq gen_code_const_def_list(const_def_list_t cdl);

// Generate code for a single constant definition
code_seq gen_code_const_def(const_def_t def);

// Generate code for variable declarations
code_seq gen_code_var_decls(var_decls_t vds);

// Generate code for a single variable declaration
code_seq gen_code_var_decl(var_decl_t vd);

// Generate code for identifiers
code_seq gen_code_idents(ident_list_t ids);

// Generate code for a single identifier
code_seq gen_code_ident(ident_t id);

// Generate code for a list of statements
code_seq gen_code_stmts(stmts_t stmts);

// Generate code for a single statement
code_seq gen_code_stmt(stmt_t *stmt);

// Generate code for an assignment statement
code_seq gen_code_assignStmt(assign_stmt_t stmt);

// Generate code for a call statement
code_seq gen_code_callStmt(call_stmt_t stmt);

// Generate code for an if statement
code_seq gen_code_ifStmt(if_stmt_t stmt);

// Generate code for a while statement
code_seq gen_code_whileStmt(while_stmt_t stmt);

// Generate code for a read statement
code_seq gen_code_readStmt(read_stmt_t stmt);

// Generate code for a print statement
code_seq gen_code_printStmt(print_stmt_t stmt);

// Generate code for a block statement
code_seq gen_code_blockStmt(block_stmt_t block_stmt);

// Generate code for an expression
code_seq gen_code_expr(expr_t exp);

// Generate code for a binary operation expression
code_seq gen_code_binary_op_expr(binary_op_expr_t exp);

// Generate code for a logical NOT expression
code_seq gen_code_logical_not_expr(negated_expr_t exp);

// Generate code for a relational operation
code_seq gen_code_rel_op(token_t rel_op);

// Generate code for an arithmetic operation
code_seq gen_code_arith_op(token_t arith_op);

// Generate code for a number
code_seq gen_code_number(number_t num);

// Generate code for an operator
code_seq gen_code_op(token_t op);

#endif // GEN_CODE_H
