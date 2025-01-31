 /*$Id: gen_code.c, 2024/11/24 $ */
#include "gen_code.h"
#include "code_seq.h"
#include "code_utils.h"
#include "ast.h"
#include "bof.h"
#include "spl.tab.h"
#include "code.h"
#include "id_use.h"
#include "literal_table.h"
#include "utilities.h"
#include "regname.h"
#include <limits.h>
#include <string.h>


#define MAX_STACK 4096

// initialize code generator
void gen_code_initialize()
{
    //debug_print("Initializing literal table...\n");
    literal_table_initialize();
    //debug_print("Literal table initialized.\n");

}

// write all instructions in cs to bf in order
static void gen_code_output_seq(BOFFILE bf, code_seq cs)
{
    while (!code_seq_is_empty(cs)) 
    {
	    bin_instr_t inst = code_seq_first(cs)->instr;
        // write instruction to file
	    instruction_write_bin_instr(bf, inst);
        // move to next instruction
	    cs = code_seq_rest(cs);
    }
}

// return a header for BOF
static BOFHeader gen_code_program_header(code_seq main_cs)
{
    BOFHeader ret;
    bof_write_magic_to_header(&ret);
    ret.text_start_address = 0;
    ret.text_length = code_seq_size(main_cs);
    int dsa = MAX(ret.text_length, 1024);
    ret.data_start_address = dsa;
    ret.data_length = literal_table_size();
    int sba = dsa + ret.data_start_address + MAX_STACK;
    ret.stack_bottom_addr = sba;
    return ret;
}

// write all literals in literal table to BOFFILE
static void gen_code_output_literals(BOFFILE bf)
{
    literal_table_start_iteration();
    while (literal_table_iteration_has_next()) 
    {
        // get next literal
        word_type w = literal_table_iteration_next();

         //debug_print("Writing literal %f to BOF file\n", w);
        // write literal to BOFFILE
        bof_write_word(bf,w);
    }
    literal_table_end_iteration();
}

// write program's BOFFILE to bf
static void gen_code_output_program(BOFFILE bf, code_seq main_cs)
{
    // generate header
    BOFHeader bfh = gen_code_program_header(main_cs);
    bof_write_header(bf, bfh);
    // write sequence of instructions
    gen_code_output_seq(bf, main_cs);
    // write literals
    gen_code_output_literals(bf);
    bof_close(bf);
}


// generate code for prog into bf
void gen_code_program(BOFFILE bf, block_t prog) 
{
    //debug_print("gen_code_program: stmts_kind = %d\n", prog.stmts.stmts_kind);
    code_seq main_cs = code_seq_empty();

    // setup code sequence
    code_seq setup_cs = code_utils_set_up_program();
    code_seq_concat(&main_cs, setup_cs);

    
    code_seq block_cs = gen_code_block(prog);
    code_seq_concat(&main_cs, block_cs);


    // teardown code sequence
    code_seq teardown_cs = code_utils_tear_down_program();
    code_seq_concat(&main_cs, teardown_cs);

    // output program to BOFFILE
    gen_code_output_program(bf, main_cs);
}

// generate code for block (variable declarations, constants, statements)
code_seq gen_code_block(block_t block) 
{
    //debug_print("Entering block\n");
    code_seq ret =  gen_code_var_decls(block.var_decls);

    
    // generate code for constant declaration
    code_seq const_decls_cs = gen_code_const_decls(block.const_decls);
    code_seq_concat(&ret, const_decls_cs);
    
    code_seq_concat(&ret, code_utils_save_registers_for_AR());
    /*Process procedure declarations in reverse order
    code_seq proc_decls_cs = gen_code_proc_decls(block.proc_decls);
    code_seq_concat(&block_cs, proc_decls_cs);
    */ 

   // generate code for statements
   //debug_print("Before gen_code_stmts: stmts_kind = %d\n", block.stmts.stmts_kind);

    code_seq stmt_cs = gen_code_stmts(block.stmts);
    code_seq_concat(&ret, stmt_cs);


    code_seq_concat(&ret, code_utils_restore_registers_from_AR());

    
    int var_len = (code_seq_size(ret) / 2);
    int const_len = ((code_seq_size(ret) - var_len) / 3);
    int total_len = const_len + var_len;

    code_utils_deallocate_stack_space(total_len);

    return ret;
}

// generate code for constant declarations
code_seq gen_code_const_decls(const_decls_t const_decls) 
{
    code_seq ret = code_seq_empty();
    const_decl_t *cdp = const_decls.start;

    while (cdp != NULL) 
    {
        // generate code for first const decl
        code_seq decl_cs = gen_code_const_decl(*cdp);
        // add to code sequence
        code_seq_concat(&decl_cs, ret);
        // move to next const decl
        cdp = cdp->next;
        ret= decl_cs;
    }

    return ret;
}

// generate code for a single constant declaration
code_seq gen_code_const_decl(const_decl_t cd) 
{
    return gen_code_const_def_list(cd.const_def_list);
}

// generate code for list of constant definitions
code_seq gen_code_const_def_list(const_def_list_t cdl) 
{
    code_seq ret = code_seq_empty();
    const_def_t *cdf = cdl.start;

    while (cdf != NULL) 
    {
        // generate code for first
        code_seq def_cs = gen_code_const_def(*cdf);
        // add to code sequence
        code_seq_concat(&def_cs, ret);
        // move to next const decl
        cdf = cdf ->next;
        ret = def_cs;
    }

    return ret;
}

code_seq gen_code_const_def(const_def_t def) {
    code_seq ret =  gen_code_number(def.number); // def.number holds the constant's value

    return ret;
}


// generate code for var_decls_t vds to out
code_seq gen_code_var_decls(var_decls_t vds)
{
    code_seq ret = code_seq_empty();
    var_decl_t *vdp = vds.var_decls;

    while (vdp != NULL) 
    {
        // generate code for single
        code_seq varDecl = gen_code_var_decl(*vdp);
        // add to code sequence
	    code_seq_concat(&varDecl, ret);

        vdp = vdp->next;

        // move to next var decl
        ret = varDecl;
    }
    return ret;
}

// generate code for single <var-decl>, vd
code_seq gen_code_var_decl(var_decl_t vd)
{
    return gen_code_idents(vd.ident_list);
}

//FIX ENTIRE FUNCTION
// generate code for identififers in idents with type vt in reverse
code_seq gen_code_idents(ident_list_t ids)
{
    code_seq ret = code_seq_empty();
    ident_t *idp = ids.start;
    
    while (idp != NULL) 
    {
	    // allocate space
	    code_seq alloc = code_utils_allocate_stack_space(1);
        code_seq_concat(&alloc, ret);

        // store ident value
        code_seq store = code_seq_singleton(code_lit(SP, 0, 0));  
        code_seq_concat(&store, ret);
        // move to next ident
        idp = idp->next;

        ret = store;
    }
    return ret;
}

//FIX
// generate code to put value of given identifier
code_seq gen_code_ident(ident_t id) {
    assert(id.idu != NULL); 
    id_attrs *attrs = id_use_get_attrs(id.idu);
    id_use *idu = id.idu;
    assert(attrs != NULL);

    unsigned int levelsOutward = idu->levelsOutward;
    unsigned int offset = attrs->offset_count;
    assert(offset <= USHRT_MAX); 

    code_seq ret = code_seq_empty();
    code_seq compute_fp = code_utils_compute_fp(4, levelsOutward);
    code_seq_concat(&ret, compute_fp);

    code_seq alloc_stack = code_utils_allocate_stack_space(1); // Allocate stack space
    code_seq_concat(&ret, alloc_stack);
    
    code_seq push_value = code_seq_singleton(code_cpw(SP, 0, 4, offset));  // Copy value onto stack
    code_seq_concat(&ret, push_value);

    return ret;
}

// generate code for list of statments given by stmts
code_seq gen_code_stmts(stmts_t stmts) 
{
    //debug_print("looking up statements\n");
    code_seq ret = code_seq_empty();
    //debug_print("Statement kind: %d\n", stmts.stmts_kind);

    if (stmts.stmts_kind != empty_stmts_e) 
    {
        //debug_print("Statements kind: %d\n", stmts.stmts_kind);
        stmt_t *stmt = stmts.stmt_list.start;
        while (stmt != NULL) 
        {
            //debug_print("lets check this stmt");
            // generate for single stmt
            code_seq stmt_cs = gen_code_stmt(stmt);
            // add to code sequence
            code_seq_concat(&ret, stmt_cs); 
            // move to next stmt

            stmt = stmt->next;
        }
    }
    //debug_print("parsed through statements");

    return ret;
}

// generate code for stmt
code_seq gen_code_stmt(stmt_t *stmt) 
{
    code_seq result = code_seq_empty();

    switch (stmt->stmt_kind) 
    {
        case assign_stmt:
            result = gen_code_assignStmt(stmt->data.assign_stmt);
            break;
        case call_stmt:
            result = gen_code_callStmt(stmt->data.call_stmt);
            break;
        case block_stmt:
            result = gen_code_blockStmt(stmt->data.block_stmt);
            break;

        case while_stmt:
            result = gen_code_whileStmt(stmt->data.while_stmt);
            break;
            
        case if_stmt:
            result = gen_code_ifStmt(stmt->data.if_stmt);
            break;
            
        case read_stmt:
            result = gen_code_readStmt(stmt->data.read_stmt);
            break;
        case print_stmt:
            result = gen_code_printStmt(stmt->data.print_stmt);
            break;
        default:
            bail_with_error("Call to gen_code_stmt with an AST that is not a statement!");
    }

    return result;
}

// *** generate code for stmts ***
code_seq gen_code_assignStmt(assign_stmt_t  stmt) {
    code_seq ret = gen_code_expr(*(stmt.expr));

    id_use *scopeHunter = stmt.idu;
    assert(scopeHunter!= NULL);
    assert(id_use_get_attrs(scopeHunter) != NULL);

    unsigned int levelsOut = scopeHunter->levelsOutward;
    code_seq fp_cs = code_utils_compute_fp(5, levelsOut);
    code_seq_concat(&ret, fp_cs);
    
    unsigned int offset_count = id_use_get_attrs(stmt.idu)->offset_count;

    // Store value
    code_seq store_cs = code_seq_singleton(code_cpw(5, offset_count, SP, 0));
    code_seq_concat(&ret, store_cs);

    // Deallocate stack space used by expression
    code_seq dealloc_cs = code_utils_deallocate_stack_space(1);
    code_seq_concat(&ret, dealloc_cs);

    return ret;
}

code_seq gen_code_callStmt(call_stmt_t stmt) 
{
    code_seq ret = code_seq_empty();

    assert(stmt.idu != NULL);

    id_use *idu = stmt.idu;
    id_attrs *attrs = id_use_get_attrs(idu);
    assert(attrs != NULL);

    unsigned int levelsOutward = idu->levelsOutward;
    unsigned int offset = attrs->offset_count;
   
    // generate static link
    code_seq static_link = code_utils_compute_fp(GP, levelsOutward);
    code_seq_concat(&ret, static_link);

    // save registers
    code_seq_concat(&ret, code_utils_save_registers_for_AR());

   // add call instr
    code_seq_add_to_end(&ret, code_call(offset));

    // restore registers
    code_seq_concat(&ret, code_utils_restore_registers_from_AR());

    return ret;
}

code_seq gen_code_condition(condition_t cond) {
    code_seq ret = code_utils_allocate_stack_space(1);

    switch (cond.cond_kind) {
        case ck_rel:
            // For relational conditions, generate code for the full expression
            ret = gen_code_expr(cond.data.rel_op_cond.expr1);
            code_seq_concat(&ret, gen_code_expr(cond.data.rel_op_cond.expr2));
            
            // Add relational operation code
            code_seq rel_op_code = gen_code_rel_op(cond.data.rel_op_cond.rel_op);
            code_seq_concat(&ret, rel_op_code);
            break;
        
        case ck_db:
            // For double binary conditions, generate code for dividend and divisor expressions
            ret = gen_code_expr(cond.data.db_cond.dividend);
            code_seq divisor_code = gen_code_expr(cond.data.db_cond.divisor);
            code_seq_concat(&ret, divisor_code);

            // Perform division
            code_seq_add_to_end(&ret, code_div(SP, 1));
            code_seq_add_to_end(&ret, code_cflo(SP, 1));
            break;
        
        default:
            bail_with_error("Unexpected condition kind (%d) in gen_code_condition", cond.cond_kind);
            break;
    }

    code_seq_concat(&ret, code_utils_deallocate_stack_space(1));
    return ret;
}


code_seq gen_code_ifStmt(if_stmt_t stmt) {
    // Allocate stack space for condition result
    code_seq ret = code_utils_allocate_stack_space(1);

    // Generate code for the condition
    code_seq condition_code = gen_code_condition(stmt.condition);
    code_seq_concat(&ret, condition_code);

    // Branching logic 
    // Use a branch if zero to skip the "then" block
    code_seq_add_to_end(&ret, code_beq(SP, 0, 2)); 

    // Code sequence for the "then" statements
    assert(stmt.then_stmts != NULL);
    code_seq then_code = gen_code_stmts(*(stmt.then_stmts));
    int then_code_size = code_seq_size(then_code);
    code_seq_concat(&ret, then_code);

    // Generate code for the "else" statements (if present)
    if (stmt.else_stmts != NULL) {
        // Add jump to skip "else" part after executing "then" statements
        code_seq_add_to_end(&ret, code_jrel(then_code_size + 1));

        code_seq else_code = gen_code_stmts(*(stmt.else_stmts));
        code_seq_concat(&ret, else_code);
    }

    // Deallocate the stack space used by the condition
    code_seq_concat(&ret, code_utils_deallocate_stack_space(1));

    return ret;
}


code_seq gen_code_whileStmt(while_stmt_t stmt) {
    code_seq ret = code_seq_empty();

    // Step 1: Mark the start of the loop
    int loop_start_offset = code_seq_size(ret);

    // Step 2: Generate code for the condition
    code_seq condition_code = gen_code_condition(stmt.condition);
    code_seq_concat(&ret, condition_code);

    // Step 3: Add a branch instruction to exit the loop if condition is false
    // Use a forward jump with a placeholder offset
    code *branch_instr = code_beq(SP, 0, 0); // Branch if zero (false)
    code_seq_add_to_end(&ret, branch_instr);

    // Step 4: Generate code for the loop body
    assert(stmt.body != NULL);
    code_seq body_code = gen_code_stmts(*(stmt.body));
    code_seq_concat(&ret, body_code);

    // Step 5: Add jump back to the start of the loop
    int back_jump_offset = -(code_seq_size(ret) - loop_start_offset + 1);
    code_seq_add_to_end(&ret, code_jrel(back_jump_offset));

    // Step 6: Patch the branch instruction to skip the loop body
    int loop_exit_offset = code_seq_size(ret) - (loop_start_offset + code_seq_size(condition_code) + 1);
    branch_instr->instr.immed.immed = loop_exit_offset;

    return ret;
}

code_seq gen_code_readStmt(read_stmt_t stmt) 
{
    code_seq ret = code_seq_empty();
    
    assert(stmt.idu != NULL);
    assert(id_use_get_attrs(stmt.idu) != NULL);

    // Compute levels outward and compute fp.
    unsigned int levelsOutward = stmt.idu->levelsOutward;
    code_seq fp_cs = code_utils_compute_fp(5, levelsOutward);
    code_seq_concat(&ret, fp_cs);

    // Compute offset
    unsigned int offset = id_use_get_attrs(stmt.idu)->offset_count;
    assert(offset <= USHRT_MAX);

    // Generate read instruction to the computed frame pointer
    code *read_instr = code_rch(5, offset); 
    code_seq_add_to_end(&ret, read_instr);

    return ret;
}

code_seq gen_code_printStmt(print_stmt_t stmt) {

    code_seq ret = gen_code_expr(stmt.expr);

    code_seq pint_cs = code_seq_singleton(code_pint(SP, 0));
    code_seq_concat(&ret, pint_cs);

    code_seq dealloc_cs = code_utils_deallocate_stack_space(1);
    code_seq_concat(&ret, dealloc_cs);

    return ret;
}


code_seq gen_code_blockStmt(block_stmt_t block_stmt) {
  
    if (block_stmt.block == NULL) {
        return code_seq_empty(); 
    }

    return gen_code_block(*(block_stmt.block));
}

code_seq gen_code_expr(expr_t exp)
{
    switch (exp.expr_kind) 
    {
        case expr_bin:
	        return gen_code_binary_op_expr(exp.data.binary);
	        break;
        case expr_ident:
	        return gen_code_ident(exp.data.ident);
	        break;
        case expr_number:
	        return gen_code_number(exp.data.number);
	        break;
        case expr_negated:
	        return gen_code_logical_not_expr(exp.data.negated);
	        break;
        default:
	        bail_with_error("Unexpected expr_kind_e (%d) in gen_code_expr", exp.expr_kind);
	        break;
    }
    // never happens, but suppresses a warning from gcc
    return code_seq_empty();
}

// generate code for expression exp
code_seq gen_code_binary_op_expr(binary_op_expr_t exp) 
{
    // First operand
    code_seq ret = gen_code_expr(*(exp.expr1));
    
    // Second operand
    code_seq expr2_code = gen_code_expr(*(exp.expr2));
    code_seq_concat(&ret, expr2_code);
    
    // Apply Op
    code_seq op_code = gen_code_op((exp.arith_op));
    code_seq_concat(&ret, op_code);

    return ret;
}

// generate code to apply op to 2nd from top and top of the stack
code_seq gen_code_op(token_t op) 
{
    switch (op.code) 
    {
        case eqsym: case neqsym:
        case ltsym: case leqsym:
        case gtsym: case geqsym:
	        return gen_code_rel_op(op);
	        break;
        case plussym: case minussym: 
        case multsym: case divsym:
	        return gen_code_arith_op(op);
	        break;
        default:
	        bail_with_error("Unknown token code (%d) in gen_code_op", op.code);
	        break;
    }
    //bail_with_error("TODO: no implementation of gen_code_op yet!");
    return code_seq_empty();
}

code_seq gen_code_arith_op(token_t arith_op) {
    code_seq do_op = code_seq_empty();
    switch (arith_op.code) {
        case plussym:
            code_seq_add_to_end(&do_op, code_add(SP, 1, SP, 0));
            break;
        case minussym:
            code_seq_add_to_end(&do_op, code_sub(SP, 0, SP, 1));
            break;
        case multsym:
            code_seq_add_to_end(&do_op, code_mul(SP, 1));
            code_seq_add_to_end(&do_op, code_cflo(SP, 1));
            break;
        case divsym:
            code_seq_add_to_end(&do_op, code_div(SP, 1));
            code_seq_add_to_end(&do_op, code_cflo(SP, 1));
            break;
        default:
            bail_with_error("Unexpected arithOp (%d) in gen_code_arith_op", arith_op.code);
            break;
    }
    // Deallocate one word from stack after operation
    code_seq dealloc_cs = code_utils_deallocate_stack_space(1);
    code_seq_concat(&do_op, dealloc_cs);
    return do_op;
}

// generate code for rel_op
code_seq gen_code_rel_op(token_t rel_op)
{
    code_seq do_op = code_seq_empty();
    switch (rel_op.code) 
    {
        case eqsym: 
	        code_seq_add_to_end(&do_op, code_beq(SP, 0, 1));
	        break;

        case neqsym:
            code_seq_add_to_end(&do_op, code_bne(SP, 0, 1));
	        break;

        case ltsym:
            code_seq_add_to_end(&do_op, code_sub(SP, 0, SP, 1));
	        code_seq_add_to_end(&do_op, code_bltz(SP, 0, 1));
	        break;

        case leqsym:
            code_seq_add_to_end(&do_op, code_sub(SP, 0, SP, 1));
	        code_seq_add_to_end(&do_op, code_blez(SP, 0, 1));
	        break;

        case gtsym:
            code_seq_add_to_end(&do_op, code_sub(SP, 0, SP, 1));
	        code_seq_add_to_end(&do_op, code_bgtz(SP, 0, 1));
	        break;

        case geqsym:
            code_seq_add_to_end(&do_op, code_sub(SP, 0, SP, 1));
	        code_seq_add_to_end(&do_op, code_bgez(SP, 0, 1));
	        break;

        default:
	        bail_with_error("Unknown token code (%d) in gen_code_rel_op", rel_op.code);
	        break;
    }
    //bail_with_error("TODO: no implementation of gen_code_rel_op yet!");
    return do_op;
}

// generate code to put given number on top of stack
code_seq gen_code_number(number_t num) {
    code_seq ret = code_utils_allocate_stack_space(1);
    unsigned int global_offset = literal_table_lookup(num.text, num.value);
    //printf("gen_code_number: num.text=%s, num.value=%d, global_offset=%u\n", num.text, num.value, global_offset);

    code_seq load_cs = code_seq_singleton(code_cpw(SP, 0, GP, global_offset));
    //debug_print("This is the sp value: %d", SP);
    code_seq_concat(&ret, load_cs);
    return ret;
}

code_seq gen_code_logical_not_expr(negated_expr_t negated) {
    // Generate code for the operand
    code_seq operand_code = gen_code_expr(*(negated.expr));
    code_seq ret = code_utils_allocate_stack_space(1);
    code_seq_add_to_end(&ret, code_neg(SP, 0, GP, 0));
    
    return ret;
}


