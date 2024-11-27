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
    code_seq ret = code_seq_empty();

    // generate code for variable declarations
    code_seq var_decls_cs = gen_code_var_decls(block.var_decls);
    code_seq_concat(&ret, var_decls_cs);
    
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

    if (cdp != NULL) 
    {
        // generate code for first const decl
        code_seq decl_cs = gen_code_const_decl(*cdp);
        // add to code sequence
        code_seq_concat(&ret, decl_cs);
        // move to next const decl
        cdp = cdp->next;
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

    if (cdf != NULL) 
    {
        // generate code for first
        code_seq def_cs = gen_code_const_def(*cdf);
        // add to code sequence
        code_seq_concat(&ret, def_cs);
        // move to next const decl
        cdf = cdf ->next;
    }

    return ret;
}

// generate code for single constant definition
code_seq gen_code_const_def(const_def_t def) 
{
    code_seq ret = code_seq_empty();

    const char * name = def.ident.name; // get name
    word_type num = def.number.value; // get value

    unsigned int literal_offset = literal_table_lookup(name, num); // get offset
    //debug_print("Adding literal: %s = %d\n", name, num);
    // allocate space
    code_seq alloc_cs = code_utils_allocate_stack_space(1);
    code_seq_concat(&ret, alloc_cs);
    
    // load const value from literal table
    code_seq load_cs = code_seq_singleton(code_lit(GP, 0, literal_offset));
    code_seq_concat(&ret, load_cs);

    // store const value
    code_seq store_cs = code_seq_singleton(code_swr(FP, 0, GP));
    code_seq_concat(&ret, store_cs);

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
        // move to next var decl
	    vdp = vdp->next;
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
        code_seq_concat(&ret, alloc);

        // store ident value
        code_seq store = code_seq_singleton(code_swr(SP, 0, 0));  
        code_seq_concat(&ret, store);

        // move to next ident
        idp = idp->next;
    }
    return ret;
}

//FIX
// generate code to put value of given identifier
code_seq gen_code_ident(ident_t id) 
{
    assert(id.idu != NULL); 
    id_attrs *attrs = id_use_get_attrs(id.idu);
    id_use *idu = id.idu;
    assert(attrs != NULL);

    unsigned int levelsOutward = idu->levelsOutward;
    unsigned int offset = attrs->offset_count;
    assert(offset <= USHRT_MAX); 

    code_seq ret = code_seq_empty();
 
    code_seq compute_fp = code_utils_compute_fp(GP, levelsOutward);
    code_seq_concat(&ret, compute_fp);

    code_seq push_value = code_seq_singleton(code_cpw(SP, 0, GP, offset));
    code_seq_concat(&ret, push_value);

    return ret;
}

// generate code for list of statments given by stmts
code_seq gen_code_stmts(stmts_t stmts) 
{
    //debug_print("looking up statements\n");
    code_seq stmts_cs = code_seq_empty();
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
            code_seq_concat(&stmts_cs, stmt_cs); 
            // move to next stmt
            stmt = stmt->next; 
        }
    }
    //debug_print("parsed through statements");

    return stmts_cs;
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
        /*
        case while_stmt:
            result = gen_code_whileStmt(&stmt->data.while_stmt);
            break;
        */
            
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

code_seq gen_code_assignStmt(assign_stmt_t  stmt)
{
    code_seq ret = gen_code_expr(*(stmt.expr));

    assert(stmt.idu != NULL);
    assert(id_use_get_attrs(stmt.idu) != NULL);

    // get offset
    unsigned int offset_count = id_use_get_attrs(stmt.idu)->offset_count;
    assert(offset_count <= USHRT_MAX);

    // store value
    code_seq store_cs = code_seq_singleton(code_swr(FP, offset_count, GP));
    code_seq_concat(&ret, store_cs);

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
code_seq gen_code_ifStmt(if_stmt_t stmt) {
    
    code_seq ret = code_seq_empty();

    condition_t condition = stmt.condition;
  
    assert(condition.cond_kind == ck_rel);

    code_seq operand1 = gen_code_expr(condition.data.rel_op_cond.expr1);
    code_seq_concat(&ret, operand1);

    code_seq operand2 = gen_code_expr(condition.data.rel_op_cond.expr2);
    code_seq_concat(&ret, operand2);

    code_seq sub_code = code_seq_singleton(code_sub(GP, 0, FP, 0));
    code_seq_concat(&ret, sub_code);

    int else_label = code_seq_size(ret) + 1;
    int end_label = else_label + 1;

    code_seq_concat(&ret, code_seq_singleton(code_bltz(GP, 0, else_label)));
    code_seq_concat(&ret, code_seq_singleton(code_beq(GP, 0, else_label)));

    // Pass then_stmts directly
    assert(stmt.then_stmts != NULL);
    code_seq then_code = gen_code_stmts(*(stmt.then_stmts));
    code_seq_concat(&ret, then_code);

    code_seq_concat(&ret, code_seq_singleton(code_jrel(end_label)));

    // Pass else_stmts directly
    if (stmt.else_stmts != NULL) {
        code_seq else_code = gen_code_stmts(*(stmt.else_stmts));
        code_seq_concat(&ret, else_code);
    }

    return ret;
}

/*code_seq gen_code_if_stmt(if_stmt_t * stmt) {
    /*code_seq ret = gen_code_condition(&(stmt->condition));
    code_seq then_code = gen_code_stmts(&(stmt->then_stmts));
    int then_code_len = code_seq_size(then_code);

    code_seq else_code = code_seq_empty();
    int else_code_len = 0;
    if (stmt->else_stmts != NULL && stmt->else_stmts.stmts_kind != empty_stmts_e) {
        else_code = gen_code_stmts(stmt->else_stmts);
        else_code_len = code_seq_size(else_code);
    }

    code_seq_add_to_end(&ret, code_beq(SP, 0, then_code_len));
    code_seq_concat(&ret, then_code);

    if (else_code_len > 0) {
        code_seq_add_to_end(&ret, code_jump(else_code_len));
    }

    code_seq_concat(&ret, else_code);//

    return ret;
    bail_with_error("Can't run gen_code_if() yet");
    return code_seq_empty();
}*/



code_seq gen_code_whileStmt(while_stmt_t stmt) {
   /* code_seq ret = code_seq_empty();
    label *start_label = label_create();
    label *end_label = label_create();

    label_set(start_label, code_seq_size(ret));

    code_seq condition_code = gen_code_condition(&(stmt->condition));
    code_seq_concat(&ret, condition_code);

    code_seq_add_to_end(&ret, code_beq(SP, 0, label_read(end_label)));

    code_seq body_code = gen_code_stmts(&stmt->body);
    code_seq_concat(&ret, body_code);

    code_seq_add_to_end(&ret, code_jmpa(label_read(start_label)));

    label_set(end_label, code_seq_size(ret));

    return ret;
    */
   bail_with_error("TODO: no implementation of gen_code_while() yet!");
   return code_seq_empty();

}

code_seq gen_code_readStmt(read_stmt_t stmt) 
{
    code_seq ret = code_seq_empty();
    
    assert(stmt.idu != NULL);
    assert(id_use_get_attrs(stmt.idu) != NULL);

    // get offset
    unsigned int offset = id_use_get_attrs(stmt.idu)->offset_count;
    assert(offset <= USHRT_MAX);

    // generate read instr
    code *read_instr = code_rch(FP, offset); 
    code_seq_add_to_end(&ret, read_instr);

    return ret;
}

code_seq gen_code_printStmt(print_stmt_t stmt)
{
    code_seq ret = code_seq_empty();

    code_seq expr_cs = gen_code_expr(stmt.expr); 
    code_seq_concat(&ret, expr_cs);

    code_seq alloc_cs = code_utils_allocate_stack_space(1);
    code_seq_concat(&ret, alloc_cs);

    code_seq pint_cs = code_seq_singleton(code_pint(SP, 0));
    code_seq_concat(&ret, pint_cs);

    code_seq dealloc_cs = code_utils_deallocate_stack_space(2);
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
    code_seq ret = gen_code_expr(*(exp.expr1)); // first operand
    code_seq_concat(&ret, gen_code_expr(*(exp.expr2))); // second operand
    code_seq_concat(&ret, gen_code_op((exp.arith_op))); // operation

    //bail_with_error("TODO: no implementation of gen_code_binary_op_expr yet!");

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

// generate code for floating-point arith_op
code_seq gen_code_arith_op(token_t arith_op) 
{
    code_seq do_op = code_seq_empty();
    switch (arith_op.code) 
    {
        case plussym:
	        code_seq_add_to_end(&do_op, code_add(SP, 1, SP, 0));
	        break;
        case minussym:
	        code_seq_add_to_end(&do_op, code_sub(SP, 1, SP, 0));
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

    //bail_with_error("TODO: no implementation of gen_code_arith_op yet!");
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
code_seq gen_code_number(number_t num)
{
    code_seq ret = code_seq_empty();
    unsigned int global_offset = literal_table_lookup(num.text, num.value);
    //debug_print("Adding literal: %s = %d\n", num.text, num.value);
    code_seq_concat(&ret, code_seq_singleton(code_cpw(SP, 0, GP, global_offset)));

    return ret;
    //bail_with_error("TODO: no implementation of gen_code_number yet!");
}

// generate code for expression exp
code_seq gen_code_logical_not_expr(negated_expr_t exp)
{
    /*
    code_seq ret = gen_code_expr(exp);
    code_seq_concat(&ret, code_pop_stack_into_reg(AT, bool_te));
    code_seq_add_to_end(&ret, code_beq(0, AT, 2));
    code_seq_add_to_end(&ret, code_add(0, 0, AT));
    code_seq_add_to_end(&ret, code_beq(0, 0, 1));
    code_seq_add_to_end(&ret, code_addi(0, AT, 1));
    code_seq_concat(&ret, code_push_reg_on_stack(AT, bool_te));
    return ret;
    */
    bail_with_error("TODO: no implementation of gen_code_logical_not_expr yet!");
    return code_seq_empty();
}
