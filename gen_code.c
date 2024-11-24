 /*$Id: gen_code.c, 2024/11/15 $ */
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

#define MAX_STACK 4096

// initialize the code generator
code_seq gen_code_initialize()
{
    literal_table_initialize();
}

//needs to be filled out
static void gen_code_output_seq(BOFFILE bf, code_seq cs){
    while (cs != NULL){
        bin_instr_t inst = code_seq_first(cs) -> instr;
        instruction_write_bin_instr(bf, inst);
        cs = code_seq_rest(cs);
        ret.text_length = code_seq_size(cs);

    }
}

static BOFHeader gen_code_program_header(code_seq main_cs){
    BOFHeader ret;
    strncpy(ret.magic, MAGIC, MAGIC_BUFFER_SIZE);
    ret.text_start_address = 0;
    ret.text_length = code_seq_size(main_cs);
    int dsa = MAX(ret.text_length, 1024);
    ret.data_start_address = dsa;
    int sba = dsa + ret.data_start_address + MAX_STACK;
    ret.stack_bottom_addr = sba;
     return ret;
}
static void gen_code_output_literals(BOFFILE bf);

static void gen_code_output_program(BOFFILE bf, code_seq main_cs);

void gen_code_program(BOFFILE bf, block_t prog) {
    
}

// generate code for var_decls_t vds to out
code_seq gen_code_var_decls(var_decls_t vds)
{
    code_seq ret = code_seq_empty();
    var_decl_t *vdp = vds.var_decls;
    while (vdp != NULL) 
    {
	    // generate these in reverse order,
	    // so the addressing offsets work properly
        code_seq varDecl = gen_code_var_decl(*vdp);
	    code_seq_concat(&varDecl, ret);
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
code_seq gen_code_idents(ident_list_t ids)
{
    code_seq ret = code_seq_empty();
    ident_t *idp = ids.start;
    while (idp != NULL) 
    {
	    code_seq alloc_and_init = code_seq_singleton(code_addi(SP, SP, - BYTES_PER_WORD));
	    // Generate these in revese order,
	    // so addressing works propertly
	    code_seq_concat(&alloc_and_init, ret);
	    idp = idp->next;
    }
    return ret;
}

//FIX
code_seq gen_code_ident(ident_t id)
{
    assert(id.idu != NULL);
    //code_seq ret = code_compute_fp(T9, id.idu->levelsOutward);
    assert(id_use_get_attrs(id.idu) != NULL);
    unsigned int offset_count = id_use_get_attrs(id.idu)->offset_count;
    assert(offset_count <= USHRT_MAX); // it has to fit!
    
    //return code_seq_concat(ret, code_push_reg_on_stack(V0, typ));
}


code_seq gen_code_stmt(stmt_t stmt)
{
    switch (stmt.stmt_kind) 
    {
        case assign_stmt:
	        return gen_code_assign_stmt(stmt.data.assign_stmt);
	        break;
        case block_stmt:
	        return gen_code_block_stmt(stmt.data.block_stmt);
	        break;
        case if_stmt:
	        return gen_code_if_stmt(stmt.data.if_stmt);
	        break;
        case read_stmt:
	        return gen_code_read_stmt(stmt.data.read_stmt);
	        break;
        case write_stmt:
	        return gen_code_write_stmt(stmt.data.write_stmt);
	        break;
        default:
	        bail_with_error("Call to gen_code_stmt with an AST that is not a statement!");
	        break;
    }

    return code_seq_empty();
}
*/

code_seq gen_code_assign_stmt(assign_stmt_t stmt)
{
    code_seq ret = gen_code_expr(*(stmt.expr));


    assert(stmt.idu != NULL);
    assert(id_use_get_attrs(stmt.idu) != NULL);

 
    unsigned int offset_count = id_use_get_attrs(stmt.idu)->offset_count;
    assert(offset_count <= USHRT_MAX);

    code_seq_concat(&ret, code_lwr(R4, FP, offset_count)); 
   
    code_seq_add_to_end(&ret, code_swr(R4, R3, 0));

    return ret;
}




code_seq gen_code_callStmt(call_stmt_t *stmt)
{
    code_seq ret = code_seq_empty();

    
    code_seq_concat(ret, code_utils_save_registers_for_AR());

   
    assert(stmt != NULL);
    assert(stmt->idu != NULL); 
    unsigned int offset_count = id_use_get_attrs(stmt->idu)->offset_count;
    code_seq_add_to_end(ret, code_call(offset_count));

  
    code_seq_concat(ret, code_utils_restore_registers_from_AR());

    return ret;
}


code_seq gen_code_blockStmt(block_stmt_t *block_stmt)
{
    code_seq ret = code_seq_empty();
    block_t *block = block_stmt->block;

   
    if (block->const_decls != NULL) {
        code_seq_concat(ret, gen_code_constDecls(&block->const_decls));
    }


    if (block->var_decls != NULL) {
        code_seq_concat(ret, gen_code_varDecls(&block->var_decls));
    }

    /*
    if (block->proc_decls != NULL) {
        code_seq_concat(ret, gen_code_procDecls(&block->proc_decls));
    }
    */

  
    if (block->stmts != NULL) {
        code_seq_concat(ret, gen_code_stmts(&block->stmts));
    }


    if (block->var_decls != NULL) {
        code_seq_concat(ret, code_utils_deallocate_stack_space(block->var_decls.num_vars));
    }

    return ret;
}

code_seq gen_code_stmts(stmts_t stmts)
{
    code_seq ret = code_seq_empty();
    stmt_t *sp = stmts.stmts;
    while (sp != NULL) {
	code_seq_concat(ret, gen_code_stmt(*sp));
	sp = sp->next;
    }
    return ret;
}

code_seq gen_code_condition(condition_t *cond)
{
    code_seq ret = code_seq_empty();

    switch (cond->cond_kind) {
        case ck_db:
            
            ret = gen_code_expr(&(cond->data.db_cond.dividend));
            code_seq_concat(ret, gen_code_expr(&(cond->data.db_cond.divisor)));
            code_seq_add_to_end(ret, code_mod(R3, R4)); 
            code_seq_add_to_end(ret, code_beqz(R3));    
            break;

        case ck_rel:
           
            ret = gen_code_expr(&(cond->data.rel_op_cond.expr1));
            code_seq_concat(ret, gen_code_expr(&(cond->data.rel_op_cond.expr2)));
            code_seq_add_to_end(ret, code_relop(cond->data.rel_op_cond.rel_op.code)); // Evaluate rel_op
            break;

        default:
            bail_with_error("Unknown condition kind in gen_code_condition!");
            break;
    }

    return ret;
}


code_seq gen_code_if_stmt(if_stmt_t stmt) {
    code_seq ret = gen_code_condition(&(stmt.condition));
    code_seq then_code = gen_code_stmts(stmt.then_stmts);
    int then_code_len = code_seq_size(then_code);

    code_seq else_code = code_seq_empty();
    int else_code_len = 0;
    if (stmt.else_stmts != NULL && stmt.else_stmts->stmts_kind != empty_stmts_e) {
        else_code = gen_code_stmts(stmt.else_stmts);
        else_code_len = code_seq_size(else_code);
    }

    code_seq_add_to_end(ret, code_beq(R3, 0, then_code_len));
    code_seq_concat(ret, then_code);

    if (else_code_len > 0) {
        code_seq_add_to_end(ret, code_jump(else_code_len));
    }

    code_seq_concat(ret, else_code);

    return ret;
}


code_seq gen_code_whileStmt(while_stmt_t *stmt)
{
    code_seq ret = code_seq_empty();
    code_seq condition_code = gen_code_condition(&(stmt->condition));
    code_seq body_code = gen_code_stmts(stmt->body);
    int condition_size = code_seq_size(condition_code);
    int body_size = code_seq_size(body_code);
    code_seq_concat(ret, condition_code);
    code_seq_add_to_end(ret, code_brf(body_size));
    code_seq_concat(ret, body_code);
    code_seq_add_to_end(ret, code_jump(-(condition_size + body_size)));
    return ret;
}


code_seq gen_code_readStmt(read_stmt_t *stmt)
{
    code_seq ret = code_seq_singleton(code_rch());
    assert(stmt.idu != NULL);
    code_seq_concat(ret, code_compute_fp(R4, stmt.idu->levelsOutward));
    assert(id_use_get_attrs(stmt.idu) != NULL);
    unsigned int offset_count = id_use_get_attrs(stmt.idu)->offset_count;
    assert(offset_count <= USHRT_MAX);
    code_seq_add_to_end(ret, code_seq_singleton(code_swr(R4, R3, offset_count)));
    return ret;
}


code_seq gen_code_printStmt(print_stmt_t *stmt)
{
     code_seq ret = gen_code_expr(stmt->expr);
    ret = code_seq_concat(ret, code_seq_singleton(code_pint(SP, 0)));
    ret = code_seq_concat(ret, code_utils_deallocate_stack_space(1));
    return ret;
}


code_seq gen_code_expr(expr_t exp)
{
    switch (exp.expr_kind) 
    {
        case expr_bin_op:
	        return gen_code_binary_op_expr(exp.data.binary);
	        break;
        case expr_ident:
	        return gen_code_ident(exp.data.ident);
	        break;
        case expr_number:
	        return gen_code_number(exp.data.number);
	        break;
        case expr_logical_not:
	        return gen_code_logical_not_expr(*(exp.data.logical_not));
	        break;
        default:
	        bail_with_error("Unexpected expr_kind_e (%d) in gen_code_expr", exp.expr_kind);
	        break;
    }
    // never happens, but suppresses a warning from gcc
    return code_seq_empty();
}

code_seq gen_code_binary_op_expr(binary_op_expr_t exp)
{
    code_seq ret = gen_code_expr(*(exp.expr1));
    ret = code_seq_concat(ret, gen_code_expr(*(exp.expr2)));
    ret = code_seq_concat(ret, gen_code_op(exp.op));
    return ret;
}


// generate code to apply op, put result on top of stack
code_seq gen_code_op(token_t op)
{
    switch (op.code) 
    {
        case eqsym: case neqsym:
        case ltsym: case leqsym:
        case gtsym: case geqsym:
	        return gen_code_rel_op(op, typ);
	        break;
        case plussym: case minussym: 
        case multsym: case divsym:
	        assert(typ == float_te);
	        return gen_code_arith_op(op);
	        break;
        default:
	        bail_with_error("Unknown token code (%d) in gen_code_op", op.code);
	        break;
    }
    return code_seq_empty();
}

// generate code to apply floating-point arith_op, put result on top of stack
code_seq gen_code_arith_op(token_t arith_op)
{
    //FIX CODE_SEQ_ADD_TO_END
    code_seq ret = code_pop_stack_into_reg(AT, float_te);
    // load next element of the stack into V0
    code_seq_concat(ret, code_pop_stack_into_reg(V0, float_te));

    code_seq do_op = code_seq_empty();
    switch (arith_op.code) 
    {
        case plussym:
	        do_op = code_seq_add_to_end(do_op, code_fadd(V0, AT, V0));
	        break;
        case minussym:
	        do_op = code_seq_add_to_end(do_op, code_fsub(V0, AT, V0));
	        break;
        case multsym:
	        do_op = code_seq_add_to_end(do_op, code_fmul(V0, AT, V0));
	        break;
        case divsym:
	        do_op = code_seq_add_to_end(do_op, code_fdiv(V0, AT, V0));
	        break;
        default:
	        bail_with_error("Unexpected arithOp (%d) in gen_code_arith_op", arith_op.code);
	        break;
    }
    do_op = code_seq_concat(do_op, code_push_reg_on_stack(V0, float_te));
    return code_seq_concat(ret, do_op);
}

// generate code for the rel_op, put result on top of stack
code_seq gen_code_rel_op(token_t rel_op)
{
    // load top of the stack (the second operand) into AT
    code_seq ret = code_pop_stack_into_reg(AT, typ);
    // load next element of the stack into V0
    code_seq_concat(ret, code_pop_stack_into_reg(V0, typ));

    // start out by doing the comparison
    // and skipping the next 2 instructions if it's true
    code_seq do_op = code_seq_empty();
    switch (rel_op.code) 
    {
        case eqsym: 
	        if (typ == float_te) 
            {
	            do_op = code_seq_singleton(code_bfeq(V0, AT, 2));
	        } 
            else 
            {
	            do_op = code_seq_singleton(code_beq(V0, AT, 2));
	        }
	        break;
        case neqsym:
	        if (typ == float_te) 
            {
	            do_op = code_seq_singleton(code_bfne(V0, AT, 2));
	        } 
            else 
            {
	            do_op = code_seq_singleton(code_bne(V0, AT, 2));
	        }
	        break;
        case ltsym:
	        if (typ == float_te) 
            {
	            do_op = code_seq_singleton(code_fsub(V0, AT, V0));
	            do_op = code_seq_add_to_end(do_op, code_bfltz(V0, 2));
	        } 
            else 
            {
	            do_op = code_seq_singleton(code_sub(V0, AT, V0));
	            do_op = code_seq_add_to_end(do_op, code_bltz(V0, 2));
	        }
	        break;
        case leqsym:
	        if (typ == float_te) 
            {
	            do_op = code_seq_singleton(code_fsub(V0, AT, V0));
	            do_op = code_seq_add_to_end(do_op, code_bflez(V0, 2));
	        } 
            else 
            {
	            do_op = code_seq_singleton(code_sub(V0, AT, V0));
	            do_op = code_seq_add_to_end(do_op, code_blez(V0, 2));
	        }
	        break;
        case gtsym:
	        if (typ == float_te) 
            {
	            do_op = code_seq_singleton(code_fsub(V0, AT, V0));
	            do_op = code_seq_add_to_end(do_op, code_bfgtz(V0, 2));
	        } 
            else 
            {
	            do_op = code_seq_singleton(code_sub(V0, AT, V0));
	            do_op = code_seq_add_to_end(do_op, code_bgtz(V0, 2));
	        }
	        break;
        case geqsym:
	        if (typ == float_te) 
            {
	            do_op = code_seq_singleton(code_fsub(V0, AT, V0));
	            do_op = code_seq_add_to_end(do_op, code_bfgez(V0, 2));
	        } 
            else 
            {
	            do_op = code_seq_singleton(code_sub(V0, AT, V0));
	            do_op = code_seq_add_to_end(do_op, code_bgez(V0, 2));
	        }
	        break;
        default:
	        bail_with_error("Unknown token code (%d) in gen_code_rel_op", rel_op.code);
	        break;
    }
    code_seq_concat(ret, do_op);
    // rest of the code for the comparisons
    code_seq_add_to_end(ret, code_add(0, 0, AT)); // put false in AT
    code_seq_add_to_end(ret, code_beq(0, 0, 1)); // skip next instr
    code_seq_add_to_end(ret, code_addi(0, AT, 1)); // put true in AT
    code_seq_concat(ret, code_push_reg_on_stack(AT, bool_te));
    return ret;
}

// generate code to put value of given identifier on top of stack
code_seq gen_code_ident(ident_t id)
{
    assert(id.idu != NULL);
    code_seq ret = code_compute_fp(T9, id.idu->levelsOutward);
    assert(id_use_get_attrs(id.idu) != NULL);
    unsigned int offset_count = id_use_get_attrs(id.idu)->offset_count;
    assert(offset_count <= USHRT_MAX); // it has to fit!
    type_exp_e typ = id_use_get_attrs(id.idu)->type;
    if (typ == float_te) 
    {
	    code_seq_add_to_end(ret, code_flw(T9, V0, offset_count));
    } 
    else 
    {
	    code_seq_add_to_end(ret, code_lw(T9, V0, offset_count));
    }
    return code_seq_concat(ret, code_push_reg_on_stack(V0, typ));
}

// generates code to put given num on top of stack
code_seq gen_code_number(number_t num)
{
    unsigned int global_offset = literal_table_lookup(num.text, num.value);
    return code_seq_concat(code_seq_singleton(code_flw(GP, V0, global_offset)),code_push_reg_on_stack(V0, float_te));
}

// generate code for expression exp put result on top of stack
code_seq gen_code_logical_not_expr(expr_t exp)
{
    code_seq ret = gen_code_expr(exp);
    code_seq_concat(ret, code_pop_stack_into_reg(AT, bool_te));
    // if 0 skip next 2 instructions
    code_seq_add_to_end(ret, code_beq(0, AT, 2));
    // it was 1, so put 0 in AT
    code_seq_add_to_end(ret, code_add(0, 0, AT));
    // and skip the next instruction
    code_seq_add_to_end(ret, code_beq(0, 0, 1));
    // put 1 in AT
    code_seq_add_to_end(ret, code_addi(0, AT, 1));
    // push the result on the stack
    code_seq_concat(ret, code_push_reg_on_stack(AT, bool_te));
    return ret;
}

// added from float
static void gen_code_output_literals(BOFFILE bf)
{
    literal_table_start_iteration();
    while (literal_table_iteration_has_next()) 
    {
	    word_type w = literal_table_iteration_next();
	    // debug_print("Writing literal %f to BOF file\n", w);
	    bof_write_float(bf, w);
    }
    literal_table_end_iteration(); // not necessary
}
