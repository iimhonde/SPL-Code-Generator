 /*$Id: gen_code.c, 2024/11/24 $ */
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
static void gen_code_output_seq(BOFFILE bf, code_seq cs)
{
    while (!code_seq_is_empty(cs)) {
	    bin_instr_t inst = code_seq_first(cs)->instr;
	    instruction_write_bin_instr(bf, inst);
	    cs = code_seq_rest(cs);
    }
}

static BOFHeader gen_code_program_header(code_seq main_cs){
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

static void gen_code_output_literals(BOFFILE bf){
    literal_table_start_iteration();
    while (literal_table_iteration_has_next()) {
        word_type w = literal_table_iteration_next();
        // debug_print("Writing literal %f to BOF file\n", w);
        bof_write_word(bf,w);
    }
    literal_table_end_iteration();
}

static void gen_code_output_program(BOFFILE bf, code_seq main_cs){
    BOFHeader bfh = gen_code_program_header(main_cs);
    bof_write_header(bf, bfh);
    gen_code_output_seq(bf, main_cs);
    gen_code_output_literals(bf);
    bof_close(bf);
}


void gen_code_program(BOFFILE bf, block_t *prog) {
    code_seq main_cs = code_seq_empty();

    code_seq setup_cs = code_utils_set_up_program();
    code_seq_concat(&main_cs, setup_cs);

    /*
    code_seq block_cs = gen_code_block(prog);
    code_seq_concat(&main_cs, block_cs);
    */

    code_seq teardown_cs = code_utils_tear_down_program();
    code_seq_concat(&main_cs, teardown_cs);
    gen_code_output_program(bf, main_cs);
}

code_seq gen_code_block(block_t *block) {

    code_seq ret = code_seq_empty();
    code_seq_add_to_end(&ret, code_cpr(3, FP));

    code_seq var_decls_cs = gen_code_var_decls(&block->var_decls);
    code_seq_concat(&ret, var_decls_cs);
    
    code_seq const_decls_cs = gen_code_const_decls(&block->const_decls);
    code_seq_concat(&ret, const_decls_cs);
    
    code_seq_concat(&ret, code_utils_save_registers_for_AR());

    /*Process procedure declarations in reverse order
    code_seq proc_decls_cs = gen_code_proc_decls(block.proc_decls);
    code_seq_concat(&block_cs, proc_decls_cs);
    */ 

    code_seq_concat(&ret, code_utils_restore_registers_from_AR());

    code_seq stmt_cs = gen_code_stmts(&block->stmts);
    code_seq_concat(&ret, stmt_cs);

    int var_len = (code_seq_size(ret) / 2);
    int const_len = ((code_seq_size(ret) - var_len) / 3);

    int total_len = const_len + var_len;

    code_utils_deallocate_stack_space(var_len);

    return ret;
}

code_seq gen_code_const_decls(const_decls_t *const_decls) {
    code_seq ret = code_seq_empty();
    const_decl_t *cdp = const_decls->start;

    if (cdp != NULL) {
        code_seq rest_cs = gen_code_const_decls(cdp->next);
        code_seq_concat(&ret, rest_cs);
        
        code_seq decl_cs = gen_code_const_decl(cdp);
        code_seq_concat(&ret, decl_cs);
    }

    return ret;
}



code_seq gen_code_const_decl(const_decl_t *cd) {
    return gen_code_const_def_list(&cd -> const_def_list);
}

code_seq gen_code_const_def_list(const_def_list_t *cdl) {
    code_seq ret = code_seq_empty();
    const_def_t *cdf = cdl->start;

    if (cdf != NULL) {
       
        code_seq rest_cs = gen_code_const_def_list(cdf->next);
        code_seq_concat(&ret, rest_cs);

        
        code_seq def_cs = gen_code_const_def(cdf);
        code_seq_concat(&ret, def_cs);
    }

    return ret;
}

code_seq gen_code_const_def(const_def_t *def) {
    code_seq ret = code_seq_empty();

    char * name = def->ident.name;

    word_type num = def->number.value;

    unsigned int literal_offset = literal_table_lookup(name, num);

    code_seq alloc_cs = code_utils_allocate_stack_space(1);
    code_seq_concat(&ret, alloc_cs);
    
    code_seq load_cs = code_seq_singleton(code_lit(GP, 0, literal_offset));
    code_seq_concat(&ret, load_cs);

    code_seq store_cs = code_seq_singleton(code_swr(FP, 0, GP));
    code_seq_concat(&ret, store_cs);

    return ret;
}    

// generate code for var_decls_t vds to out
code_seq gen_code_var_decls(var_decls_t *vds)
{
    code_seq ret = code_seq_empty();
    var_decl_t *vdp = vds ->var_decls;
    while (vdp != NULL) 
    {
        code_seq varDecl = gen_code_var_decl(vdp);
	    code_seq_concat(&varDecl, ret);
	    vdp = vdp->next;
    }
    return ret;
}


code_seq gen_code_var_decl(var_decl_t *vd)
{
    return gen_code_idents(&vd ->ident_list);
}

//FIX ENTIRE FUNCTION
code_seq gen_code_idents(ident_list_t *ids)
{
    code_seq ret = code_seq_empty();
    ident_t *idp = ids ->start;
    
   

    while (idp != NULL) 
    {
	    
	    code_seq alloc = code_utils_allocate_stack_space(1);
        code_seq_concat(&ret, alloc);

        code_seq store = code_seq_singleton(code_swr(SP, 0, 0));  
        code_seq_concat(&ret, store);

        idp = idp->next;
     		
    }
    return ret;
}

//FIX
code_seq gen_code_ident(ident_t *id) {
    assert(id->idu != NULL); 
    id_attrs *attrs = id_use_get_attrs(id->idu);
    id_use *idu = id -> idu;
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


code_seq gen_code_stmts(stmts_t *stmts) {
    code_seq stmts_cs = code_seq_empty();

    if (stmts->stmts_kind != empty_stmts_e) {
        stmt_t *stmt = stmts->stmt_list.start;
        while (stmt != NULL) {
            code_seq stmt_cs = gen_code_stmt(stmt);
            code_seq_concat(&stmts_cs, stmt_cs); 
            stmt = stmt->next; 
        }
    }

    return stmts_cs;
}

code_seq gen_code_stmt(stmt_t *stmt) {
    code_seq result = code_seq_empty();

    switch (stmt->stmt_kind) {
        case assign_stmt:
            result = gen_code_assignStmt(&stmt->data.assign_stmt);
            break;
        case call_stmt:
            result = gen_code_callStmt(&stmt->data.call_stmt);
            break;
        case block_stmt:
            result = gen_code_blockStmt(&stmt->data.block_stmt);
            break;
        /*
        case while_stmt:
            result = gen_code_whileStmt(&stmt->data.while_stmt);
            break;
        */
            /*
        case if_stmt:
            result = gen_code_ifStmt(&stmt->data.if_stmt);
            break;
            */
        case read_stmt:
            result = gen_code_readStmt(&stmt->data.read_stmt);
            break;
        case print_stmt:
            result = gen_code_printStmt(&stmt->data.print_stmt);
            break;
        default:
            bail_with_error("Call to gen_code_stmt with an AST that is not a statement!");
    }

    return result;
}

code_seq gen_code_assignStmt(assign_stmt_t * stmt)
{
    code_seq ret = gen_code_expr(&stmt->expr);


    assert(stmt->idu != NULL);
    assert(id_use_get_attrs(stmt->idu) != NULL);


    unsigned int offset_count = id_use_get_attrs(stmt->idu)->offset_count;
    assert(offset_count <= USHRT_MAX);

    code_seq store_cs = code_seq_singleton(code_swr(FP, offset_count, GP));
    code_seq_concat(&ret, store_cs);

    return ret;
}



code_seq gen_code_callStmt(call_stmt_t *stmt) {
    code_seq ret = code_seq_empty();

    assert(stmt->idu != NULL);

    id_use *idu = stmt->idu;
    id_attrs *attrs = id_use_get_attrs(idu);
    assert(attrs != NULL);

    unsigned int levelsOutward = idu->levelsOutward;
    unsigned int offset = attrs->offset_count;

   
    code_seq static_link = code_utils_compute_fp(GP, levelsOutward);
    code_seq_concat(&ret, static_link);

    code_seq_concat(&ret, code_utils_save_registers_for_AR());

   
    code_seq_add_to_end(&ret, code_call(offset));

 
    code_seq_concat(&ret, code_utils_restore_registers_from_AR());

    return ret;
}


code_seq gen_code_blockStmt(block_stmt_t *block_stmt) {
  
    if (block_stmt == NULL || block_stmt->block == NULL) {
        return code_seq_empty(); 
    }

    return gen_code_block(&block_stmt->block);
}



/*
code_seq gen_code_if_stmt(if_stmt_t * stmt) {
    code_seq ret = gen_code_condition(&(stmt->condition));
    code_seq then_code = gen_code_stmts(&(stmt->then_stmts));
    int then_code_len = code_seq_size(then_code);

    code_seq else_code = code_seq_empty();
    int else_code_len = 0;
    if (stmt->else_stmts != NULL && stmt->else_stmts->stmts_kind != empty_stmts_e) {
        else_code = gen_code_stmts(stmt->else_stmts);
        else_code_len = code_seq_size(else_code);
    }

    code_seq_add_to_end(&ret, code_beq(SP, 0, then_code_len));
    code_seq_concat(&ret, then_code);

    if (else_code_len > 0) {
        code_seq_add_to_end(&ret, code_jump(else_code_len));
    }

    code_seq_concat(&ret, else_code);

    return ret;
}
*/


/*
code_seq gen_code_whileStmt(while_stmt_t *stmt) {
    code_seq ret = code_seq_empty();
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
}
*/


code_seq gen_code_readStmt(read_stmt_t *stmt) {
   
    code_seq ret = code_seq_empty();

    
    assert(stmt->idu != NULL);
    assert(id_use_get_attrs(stmt->idu) != NULL);

    
    unsigned int offset = id_use_get_attrs(stmt->idu)->offset_count;
    assert(offset <= USHRT_MAX);


    code *read_instr = code_rch(FP, offset); 
    code_seq_add_to_end(&ret, read_instr);

    return ret;
}



code_seq gen_code_printStmt(print_stmt_t *stmt)
{
    code_seq ret = gen_code_expr(&stmt->expr);
    code_seq_concat(&ret, code_seq_singleton(code_pint(SP, 0)));
    code_seq_concat(&ret, code_utils_deallocate_stack_space(1));
    return ret;
}


code_seq gen_code_expr(expr_t* exp)
{
    switch (exp->expr_kind) 
    {
        case expr_bin:
	        return gen_code_binary_op_expr(&exp ->data.binary);
	        break;
        case expr_ident:
	        return gen_code_ident(&exp ->data.ident);
	        break;
        case expr_number:
	        return gen_code_number(&exp ->data.number);
	        break;
        case expr_negated:
	        return gen_code_logical_not_expr(&exp->data.negated);
	        break;
        default:
	        bail_with_error("Unexpected expr_kind_e (%d) in gen_code_expr", exp ->expr_kind);
	        break;
    }
    // never happens, but suppresses a warning from gcc
    return code_seq_empty();
}

code_seq gen_code_binary_op_expr(binary_op_expr_t *exp)
{
    /*
    code_seq ret = gen_code_expr(*(exp.expr1));
    code_seq_concat(&ret, gen_code_expr(*(exp.expr2)));
    code_seq_concat(&ret, gen_code_op(exp.op));
    return ret;
    */
    bail_with_error("TODO: no implementation of gen_code_binary_op_expr yet!");
    return code_seq_empty();
}

code_seq gen_code_op(token_t *op)
{
    /*
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
    return code_seq_empty();
    */
    bail_with_error("TODO: no implementation of gen_code_op yet!");
    return code_seq_empty();
}

code_seq gen_code_arith_op(token_t *arith_op)
{
    /*
    code_seq ret = code_pop_stack_into_reg(AT, float_te);
    code_seq_concat(&ret, code_pop_stack_into_reg(V0, float_te));

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
    return code_seq_concat(&ret, do_op);
    */
    bail_with_error("TODO: no implementation of gen_code_arith_op yet!");
    return code_seq_empty();
}

code_seq gen_code_rel_op(token_t *rel_op)
{
    /*
    code_seq ret = code_pop_stack_into_reg(AT, typ);
    code_seq_concat(&ret, code_pop_stack_into_reg(V0, typ));

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
    code_seq_concat(&ret, do_op);
    code_seq_add_to_end(&ret, code_add(0, 0, AT));
    code_seq_add_to_end(&ret, code_beq(0, 0, 1));
    code_seq_add_to_end(&ret, code_addi(0, AT, 1));
    code_seq_concat(&ret, code_push_reg_on_stack(AT, bool_te));
    return ret;
    */
    bail_with_error("TODO: no implementation of gen_code_rel_op yet!");
    return code_seq_empty();
}

code_seq gen_code_number(number_t *num)
{
    /*
    unsigned int global_offset = literal_table_lookup(num.text, num.value);
    return code_seq_concat(code_seq_singleton(code_flw(GP, V0, global_offset)), code_push_reg_on_stack(V0, float_te));
    */
    bail_with_error("TODO: no implementation of gen_code_number yet!");
    return code_seq_empty();
}

code_seq gen_code_logical_not_expr(expr_t *exp)
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
// added from float
/*
void gen_code_output_literals(BOFFILE bf)
{
    
    literal_table_start_iteration();
    while (literal_table_iteration_has_next()) 
    {
	    word_type w = literal_table_iteration_next();
	    bof_write_float(bf, w);
    }
    literal_table_end_iteration();
    
    bail_with_error("TODO: no implementation of gen_code_output_literals yet!");
}
*/