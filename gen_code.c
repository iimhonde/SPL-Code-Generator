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
        ret.text_length = code_seq_size(main_cs);

    }
}

static BOFHeader gen_code_program_header(code_seq main_cs){
    BOFHeader ret;
    strncpy(ret.magic, MAGIC, MAGIC_BUFFER_SIZE);
    ret.text_start_address = 0;

}
//static void gen_code_output_literals(BOFFILE bf)

//static void gen_code_output_program(BOFFILE bf, code_seq main_cs)

void gen_code_program(BOFFILE bf, block_t prog){

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
	    ret = code_seq_concat(gen_code_var_decl(*vdp), ret);
	    vdp = vdp->next;
    }
    return ret;
}

// generate code for single <var-decl>, vd
code_seq gen_code_var_decl(var_decl_t vd)
{
    return gen_code_idents(vd.idents, vd.type);
}

// generate code for identififers in idents with type vt
code_seq gen_code_idents(ident_list_t ids)
{
    code_seq ret = code_seq_empty();
    ident_t *idp = idents.idents;
    while (idp != NULL) 
    {
	    code_seq alloc_and_init = code_seq_singleton(code_addi(SP, SP, - BYTES_PER_WORD));
	    switch (vt) 
        {
	        case float_te:
	            alloc_and_init = code_seq_add_to_end(alloc_and_init, code_fsw(SP, 0, 0));
	            break;
	        case bool_te:
	            alloc_and_init = code_seq_add_to_end(alloc_and_init, code_sw(SP, 0, 0));
	            break;
	        default:
	            bail_with_error("Bad type_exp_e (%d) passed to gen_code_idents!", vt);
	            break;
	    }
	    // Generate these in revese order,
	    // so addressing works propertly
	    ret = code_seq_concat(alloc_and_init, ret);
	    idp = idp->next;
    }
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
	    ret = code_seq_add_to_end(ret, code_flw(T9, V0, offset_count));
    } 
    else 
    {
	    ret = code_seq_add_to_end(ret, code_lw(T9, V0, offset_count));
    }
    return code_seq_concat(ret, code_push_reg_on_stack(V0, typ));
}

// generate code for stmt
code_seq gen_code_stmt(stmt_t stmt)
{
    switch (stmt.stmt_kind) 
    {
        case assign_stmt:
	        return gen_code_assign_stmt(stmt.data.assign_stmt);
	        break;
        case begin_stmt:
	        return gen_code_begin_stmt(stmt.data.begin_stmt);
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
    // The following can never execute, but this quiets gcc's warning
    return code_seq_empty();
}

// generate code for stmt
code_seq gen_code_assign_stmt(assign_stmt_t stmt)
{
    // can't call gen_code_ident,
    // since stmt.name is not an ident_t
    code_seq ret;
    // put value of expression in $v0
    ret = gen_code_expr(*(stmt.expr));
    assert(stmt.idu != NULL);
    assert(id_use_get_attrs(stmt.idu) != NULL);
    type_exp_e typ = id_use_get_attrs(stmt.idu)->type;
    ret = code_seq_concat(ret, code_pop_stack_into_reg(V0, typ));
    // put frame pointer from the lexical address of the name
    // (using stmt.idu) into $t9
    ret = code_seq_concat(ret, code_compute_fp(T9, stmt.idu->levelsOutward));
    unsigned int offset_count = id_use_get_attrs(stmt.idu)->offset_count;
    assert(offset_count <= USHRT_MAX); // it has to fit!
    switch (id_use_get_attrs(stmt.idu)->type) 
    {
        case float_te:
	        ret = code_seq_add_to_end(ret, code_fsw(T9, V0, offset_count));
	        break;
        case bool_te:
	        ret = code_seq_add_to_end(ret, code_sw(T9, V0, offset_count));
	        break;
        default:
	        bail_with_error("Bad var_type (%d) for ident in assignment stmt!", id_use_get_attrs(stmt.idu)->type);
	        break;
    }
    return ret;
}

// *** generate code for stmts ***

code_seq gen_code_callStmt(call_stmt_t *stmt)
{
    // to do
}

code_seq gen_code_blockStmt(block_stmt_t *block)
{
    // to do
}

code_seq gen_code_ifStmt(if_stmt_t *stmt)
{
    // put truth value of stmt.expr in $v0
    code_seq ret = gen_code_expr(stmt.expr);
    ret = code_seq_concat(ret, code_pop_stack_into_reg(V0, bool_te));
    code_seq cbody = gen_code_stmt(*(stmt.body));
    int cbody_len = code_seq_size(cbody);
    // skip over body if $v0 contains false
    ret = code_seq_add_to_end(ret, code_beq(V0, 0, cbody_len));
    return code_seq_concat(ret, cbody);
}

code_seq gen_code_whileStmt(while_stmt_t *stmt)
{
    // to do
}

code_seq gen_code_readStmt(read_stmt_t *stmt)
{
    // put number read into $v0
    code_seq ret = code_seq_singleton(code_rch());
    // put frame pointer from the lexical address of the name
    // (using stmt.idu) into $t9
    assert(stmt.idu != NULL);
    ret = code_seq_concat(ret, code_compute_fp(T9, stmt.idu->levelsOutward));
    assert(id_use_get_attrs(stmt.idu) != NULL);
    unsigned int offset_count = id_use_get_attrs(stmt.idu)->offset_count;
    assert(offset_count <= USHRT_MAX); // it has to fit!
    ret = code_seq_add_to_end(ret, code_seq_singleton(code_fsw(T9, V0, offset_count)));
    return ret;
}

code_seq gen_code_printStmt(print_stmt_t *stmt)
{
    // to do
}

// generate code for the expression exp, put result on top of stack
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

// generate code for the expression exp, put result on top of stack
code_seq gen_code_binary_op_expr(binary_op_expr_t exp)
{
    // put the values of the two subexpressions on the stack
    code_seq ret = gen_code_expr(*(exp.expr1));
    ret = code_seq_concat(ret, gen_code_expr(*(exp.expr2)));
    // check the types match
    type_exp_e t1 = ast_expr_type(*(exp.expr1));
    assert(ast_expr_type(*(exp.expr2)) == t1);
    // do the operation, putting the result on the stack
    ret = code_seq_concat(ret, gen_code_op(exp.op, t1));
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
    // load top of the stack (the second operand) into AT
    code_seq ret = code_pop_stack_into_reg(AT, float_te);
    // load next element of the stack into V0
    ret = code_seq_concat(ret, code_pop_stack_into_reg(V0, float_te));

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
    ret = code_seq_concat(ret, code_pop_stack_into_reg(V0, typ));

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
    ret = code_seq_concat(ret, do_op);
    // rest of the code for the comparisons
    ret = code_seq_add_to_end(ret, code_add(0, 0, AT)); // put false in AT
    ret = code_seq_add_to_end(ret, code_beq(0, 0, 1)); // skip next instr
    ret = code_seq_add_to_end(ret, code_addi(0, AT, 1)); // put true in AT
    ret = code_seq_concat(ret, code_push_reg_on_stack(AT, bool_te));
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
	    ret = code_seq_add_to_end(ret, code_flw(T9, V0, offset_count));
    } 
    else 
    {
	    ret = code_seq_add_to_end(ret, code_lw(T9, V0, offset_count));
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
    ret = code_seq_concat(ret, code_pop_stack_into_reg(AT, bool_te));
    // if 0 skip next 2 instructions
    ret = code_seq_add_to_end(ret, code_beq(0, AT, 2));
    // it was 1, so put 0 in AT
    ret = code_seq_add_to_end(ret, code_add(0, 0, AT));
    // and skip the next instruction
    ret = code_seq_add_to_end(ret, code_beq(0, 0, 1));
    // put 1 in AT
    ret = code_seq_add_to_end(ret, code_addi(0, AT, 1));
    // push the result on the stack
    ret = code_seq_concat(ret, code_push_reg_on_stack(AT, bool_te));
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
