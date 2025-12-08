/*
 * test_program.c - Unit tests for program control flow
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include "test_framework.h"
#include "mock_acia.h"

/* Include types first */
#include "../../include/types.h"

/* Need BCD for real values */
#include "../../src/bcd.c"

/* Mock variable storage for symtab */
static uint8_t mock_var_storage[MEM_VARS_SIZE];

/* Include symtab inline */
#include "../../include/symtab.h"
symtab_t symtab;
static uint8_t *var_storage = mock_var_storage;

static void to_upper_str(char *dest, const char *src) {
    uint8_t i = 0;
    while (src[i] && i < MAX_IDENT_LEN) {
        char c = src[i];
        if (c >= 'a' && c <= 'z') {
            dest[i] = c - 'a' + 'A';
        } else {
            dest[i] = c;
        }
        i++;
    }
    dest[i] = '\0';
}

void sym_init_all(void) {
    symtab.num_symbols = 0;
    symtab.next_addr = 0;
    symtab.num_labels = 0;
    symtab.num_common_blocks = 0;
}

void sym_init(void) {
    symtab.num_symbols = 0;
    symtab.next_addr = 0;
}

symbol_t *sym_lookup(const char *name) {
    char upper_name[MAX_IDENT_LEN + 1];
    to_upper_str(upper_name, name);
    for (uint8_t i = 0; i < symtab.num_symbols; i++) {
        if (strcmp(symtab.symbols[i].name, upper_name) == 0) {
            return &symtab.symbols[i];
        }
    }
    return NULL;
}

uint16_t sym_total_size(const symbol_t *sym) {
    if (sym == NULL) return 0;
    if (sym->num_dims == 0) return 1;
    uint16_t total = 1;
    for (uint8_t i = 0; i < sym->num_dims; i++) {
        total *= sym->dims[i];
    }
    return total;
}

symbol_t *sym_add(const char *name, uint8_t type, uint8_t flags, uint16_t size) {
    if (symtab.num_symbols >= MAX_SYMBOLS) return NULL;
    if (sym_lookup(name) != NULL) return NULL;
    symbol_t *sym = &symtab.symbols[symtab.num_symbols];
    to_upper_str(sym->name, name);
    sym->type = type;
    sym->flags = flags;
    sym->addr = symtab.next_addr;
    /* For backwards compatibility: 1D array with given size */
    if (size > 1) {
        sym->num_dims = 1;
        sym->dims[0] = size;
        sym->dims[1] = 1;
        sym->dims[2] = 1;
    } else {
        sym->num_dims = 0;  /* Scalar */
        sym->dims[0] = 1;
        sym->dims[1] = 1;
        sym->dims[2] = 1;
    }
    uint16_t bytes = (type == TYPE_REAL) ? 6 : 2;
    symtab.next_addr += size * bytes;
    symtab.num_symbols++;
    return sym;
}

symbol_t *sym_add_array(const char *name, uint8_t type, uint8_t flags,
                        uint8_t num_dims, const uint16_t *dims) {
    if (symtab.num_symbols >= MAX_SYMBOLS) return NULL;
    if (sym_lookup(name) != NULL) return NULL;
    if (num_dims > MAX_DIMS) num_dims = MAX_DIMS;
    symbol_t *sym = &symtab.symbols[symtab.num_symbols];
    to_upper_str(sym->name, name);
    sym->type = type;
    sym->flags = flags | SYM_ARRAY;
    sym->addr = symtab.next_addr;
    sym->num_dims = num_dims;
    uint16_t total_size = 1;
    for (uint8_t i = 0; i < MAX_DIMS; i++) {
        if (i < num_dims && dims[i] > 0) {
            sym->dims[i] = dims[i];
            total_size *= dims[i];
        } else {
            sym->dims[i] = 1;
        }
    }
    uint16_t bytes = (type == TYPE_REAL) ? 6 : 2;
    symtab.next_addr += total_size * bytes;
    symtab.num_symbols++;
    return sym;
}

int16_t sym_get_int(symbol_t *sym, uint16_t index) {
    if (sym == NULL || index >= sym_total_size(sym)) return 0;
    uint16_t addr = sym->addr + index * 2;
    uint8_t *ptr = var_storage + addr;
    return (int16_t)(ptr[0] | (ptr[1] << 8));
}

void sym_set_int(symbol_t *sym, uint16_t index, int16_t value) {
    if (sym == NULL || index >= sym_total_size(sym)) return;
    uint16_t addr = sym->addr + index * 2;
    uint8_t *ptr = var_storage + addr;
    ptr[0] = (uint8_t)(value & 0xFF);
    ptr[1] = (uint8_t)((value >> 8) & 0xFF);
    sym->flags |= SYM_DEFINED;
}

void sym_get_real(symbol_t *sym, uint16_t index, bcd_t *value) {
    if (sym == NULL || index >= sym_total_size(sym) || value == NULL) {
        bcd_zero(value);
        return;
    }
    uint16_t addr = sym->addr + index * 6;
    uint8_t *ptr = var_storage + addr;
    value->sign = ptr[0];
    value->exp = ptr[1];
    value->digits[0] = ptr[2];
    value->digits[1] = ptr[3];
    value->digits[2] = ptr[4];
    value->digits[3] = ptr[5];
}

void sym_set_real(symbol_t *sym, uint16_t index, const bcd_t *value) {
    if (sym == NULL || index >= sym_total_size(sym) || value == NULL) return;
    uint16_t addr = sym->addr + index * 6;
    uint8_t *ptr = var_storage + addr;
    ptr[0] = value->sign;
    ptr[1] = value->exp;
    ptr[2] = value->digits[0];
    ptr[3] = value->digits[1];
    ptr[4] = value->digits[2];
    ptr[5] = value->digits[3];
    sym->flags |= SYM_DEFINED;
}

void label_init(void) { symtab.num_labels = 0; }

bool_t label_add(uint16_t label, uint16_t line, uint16_t addr) {
    if (symtab.num_labels >= MAX_LABELS) return FALSE;
    for (uint8_t i = 0; i < symtab.num_labels; i++) {
        if (symtab.labels[i].label == label) return FALSE;
    }
    label_t *lbl = &symtab.labels[symtab.num_labels];
    lbl->label = label;
    lbl->line = line;
    lbl->addr = addr;
    symtab.num_labels++;
    return TRUE;
}

label_t *label_lookup(uint16_t label) {
    for (uint8_t i = 0; i < symtab.num_labels; i++) {
        if (symtab.labels[i].label == label) return &symtab.labels[i];
    }
    return NULL;
}

/* COMMON block functions */
void common_init(void) {
    symtab.num_common_blocks = 0;
}

common_block_t *common_find_or_create(const char *name) {
    char upper_name[MAX_IDENT_LEN + 1];
    to_upper_str(upper_name, name);

    for (uint8_t i = 0; i < symtab.num_common_blocks; i++) {
        if (strcmp(symtab.common_blocks[i].name, upper_name) == 0) {
            return &symtab.common_blocks[i];
        }
    }

    if (symtab.num_common_blocks >= MAX_COMMON_BLOCKS) {
        return NULL;
    }

    common_block_t *cb = &symtab.common_blocks[symtab.num_common_blocks];
    strcpy(cb->name, upper_name);
    cb->base_addr = symtab.next_addr;
    cb->next_offset = 0;
    symtab.num_common_blocks++;

    return cb;
}

symbol_t *common_add_var(common_block_t *cb, const char *name, uint8_t type, uint16_t size) {
    if (cb == NULL) return NULL;

    symbol_t *existing = sym_lookup(name);
    if (existing != NULL) {
        if (existing->flags & SYM_COMMON) {
            return existing;
        }
        return NULL;
    }

    if (symtab.num_symbols >= MAX_SYMBOLS) {
        return NULL;
    }

    symbol_t *sym = &symtab.symbols[symtab.num_symbols];
    to_upper_str(sym->name, name);
    sym->type = type;
    sym->flags = SYM_VARIABLE | SYM_COMMON;
    sym->addr = cb->base_addr + cb->next_offset;

    if (size > 1) {
        sym->num_dims = 1;
        sym->dims[0] = size;
        sym->dims[1] = 1;
        sym->dims[2] = 1;
        sym->flags |= SYM_ARRAY;
    } else {
        sym->num_dims = 0;
        sym->dims[0] = 1;
        sym->dims[1] = 1;
        sym->dims[2] = 1;
    }

    uint16_t bytes_per_elem = (type == TYPE_INTEGER) ? 2 : 6;
    cb->next_offset += size * bytes_per_elem;

    if (cb->base_addr + cb->next_offset > symtab.next_addr) {
        symtab.next_addr = cb->base_addr + cb->next_offset;
    }

    symtab.num_symbols++;
    return sym;
}

/* Include lexer */
#include "../../src/lexer.c"

/* Mock program storage - define before including program.c */
#include "../../include/program.h"
program_t program;
static prog_line_t mock_prog_lines[MAX_PROG_LINES];

/* Redefine prog_lines to use our mock */
#undef prog_lines
#define prog_lines mock_prog_lines

/* Now include program.c with mock prog_lines */
/* We need to skip the global definition since we defined it above */
#define program program  /* prevent redefinition */

/* Helper for get_line_keyword - defined in program.c */
static token_type_t get_line_keyword(const char *line) {
    lexer_t lex;
    lexer_init(&lex, line, 0);
    if (lexer_current(&lex)->type == TK_INT_LIT) {
        lexer_next(&lex);
    }
    return lexer_current(&lex)->type;
}

/* Implement program functions inline for testing */
void prog_init(void) {
    program.num_lines = 0;
    program.current_line = 0;
    program.running = FALSE;
    program.stopped = FALSE;
    program.do_depth = 0;
    program.if_depth = 0;
    program.num_subprogs = 0;
    program.call_depth = 0;
    label_init();
}

bool_t prog_add_line(const char *line, uint16_t label) {
    if (program.num_lines >= MAX_PROG_LINES) return FALSE;
    prog_line_t *pl = &prog_lines[program.num_lines];
    pl->label = label;
    strncpy(pl->text, line, MAX_LINE_LEN);
    pl->text[MAX_LINE_LEN] = '\0';
    if (label > 0) {
        if (!label_add(label, program.num_lines, program.num_lines)) {
            return FALSE;
        }
    }
    program.num_lines++;
    return TRUE;
}

int8_t prog_find_label(uint16_t label) {
    label_t *lbl = label_lookup(label);
    if (lbl != NULL) return (int8_t)lbl->line;
    return -1;
}

const char *prog_get_line(void) {
    if (program.current_line < program.num_lines) {
        return prog_lines[program.current_line].text;
    }
    return NULL;
}

void prog_next_line(void) {
    if (program.current_line < program.num_lines) {
        program.current_line++;
    }
}

bool_t prog_goto(uint16_t label) {
    int8_t idx = prog_find_label(label);
    if (idx >= 0) {
        program.current_line = (uint8_t)idx;
        return TRUE;
    }
    return FALSE;
}

bool_t prog_push_do(uint16_t end_label, const char *var_name,
                     int16_t end_val, int16_t step_val) {
    if (program.do_depth >= MAX_DO_NEST) return FALSE;
    do_state_t *ds = &program.do_stack[program.do_depth];
    ds->end_label = end_label;
    ds->line_idx = program.current_line;
    strncpy(ds->var_name, var_name, MAX_IDENT_LEN);
    ds->var_name[MAX_IDENT_LEN] = '\0';
    ds->end_val = end_val;
    ds->step_val = step_val;
    program.do_depth++;
    return TRUE;
}

bool_t prog_pop_do(uint16_t label) {
    for (uint8_t i = program.do_depth; i > 0; i--) {
        do_state_t *ds = &program.do_stack[i - 1];
        if (ds->end_label == label) {
            symbol_t *sym = sym_lookup(ds->var_name);
            if (sym == NULL || sym->type != TYPE_INTEGER) return FALSE;
            int16_t val = sym_get_int(sym, 0);
            val += ds->step_val;
            sym_set_int(sym, 0, val);
            bool_t done;
            if (ds->step_val > 0) {
                done = (val > ds->end_val);
            } else {
                done = (val < ds->end_val);
            }
            if (!done) {
                program.current_line = ds->line_idx + 1;
                program.do_depth |= 0x80;
            } else {
                program.do_depth = i - 1;
            }
            return TRUE;
        }
    }
    return FALSE;
}

bool_t prog_push_if(bool_t condition) {
    if (program.if_depth >= MAX_IF_NEST) return FALSE;
    if_state_t *is = &program.if_stack[program.if_depth];
    is->line_idx = program.current_line;
    is->condition = condition;
    is->in_else = FALSE;
    program.if_depth++;
    return TRUE;
}

bool_t prog_pop_if(void) {
    if (program.if_depth == 0) return FALSE;
    program.if_depth--;
    return TRUE;
}

bool_t prog_handle_else(void) {
    if (program.if_depth == 0) return FALSE;
    if_state_t *is = &program.if_stack[program.if_depth - 1];
    if (is->in_else) return FALSE;
    is->in_else = TRUE;
    return !is->condition;  /* Execute ELSE if IF was false */
}

bool_t prog_at_end(void) {
    return program.current_line >= program.num_lines || program.stopped;
}

subprog_t *prog_add_subprog(const char *name, uint8_t type, uint8_t return_type) {
    if (program.num_subprogs >= MAX_SUBPROGRAMS) return NULL;
    subprog_t *sp = &program.subprogs[program.num_subprogs];
    strncpy(sp->name, name, MAX_IDENT_LEN);
    sp->name[MAX_IDENT_LEN] = '\0';
    sp->type = type;
    sp->return_type = return_type;
    sp->start_line = program.current_line + 1;
    sp->end_line = 0;
    sp->num_params = 0;
    program.num_subprogs++;
    return sp;
}

subprog_t *prog_find_subprog(const char *name) {
    for (uint8_t i = 0; i < program.num_subprogs; i++) {
        if (strcmp(program.subprogs[i].name, name) == 0) {
            return &program.subprogs[i];
        }
    }
    return NULL;
}

bool_t prog_push_call(uint8_t return_line, uint8_t subprog_idx) {
    if (program.call_depth >= MAX_CALL_DEPTH) return FALSE;
    call_frame_t *frame = &program.call_stack[program.call_depth];
    frame->return_line = return_line;
    frame->subprog_idx = subprog_idx;
    frame->saved_do_depth = program.do_depth & 0x7F;
    frame->saved_if_depth = program.if_depth;
    program.call_depth++;
    return TRUE;
}

bool_t prog_pop_call(void) {
    if (program.call_depth == 0) return FALSE;
    program.call_depth--;
    call_frame_t *frame = &program.call_stack[program.call_depth];
    program.do_depth = frame->saved_do_depth;
    program.if_depth = frame->saved_if_depth;
    program.current_line = frame->return_line;
    program.do_depth |= 0x80;
    return TRUE;
}

bool_t prog_in_subprog(void) {
    return program.call_depth > 0;
}

/*============================================================================
 * Test: Program initialization
 *============================================================================*/
void test_prog_init(void) {
    TEST("prog_init clears all state");

    program.num_lines = 5;
    program.do_depth = 3;
    prog_init();

    ASSERT_EQ(0, program.num_lines);
    ASSERT_EQ(0, program.current_line);
    ASSERT_EQ(FALSE, program.running);
    ASSERT_EQ(0, program.do_depth);
    ASSERT_EQ(0, program.if_depth);
    ASSERT_EQ(0, program.num_subprogs);
    ASSERT_EQ(0, program.call_depth);
    TEST_PASS();
}

/*============================================================================
 * Test: Adding and retrieving lines
 *============================================================================*/
void test_prog_add_line(void) {
    TEST("prog_add_line stores line");
    prog_init();

    ASSERT_TRUE(prog_add_line("X = 42", 0));
    ASSERT_EQ(1, program.num_lines);
    ASSERT_STR_EQ("X = 42", prog_get_line());
    TEST_PASS();
}

void test_prog_add_line_with_label(void) {
    TEST("prog_add_line with label");
    prog_init();

    ASSERT_TRUE(prog_add_line("10 CONTINUE", 10));
    ASSERT_EQ(0, prog_find_label(10));  /* Line 0 */
    TEST_PASS();
}

void test_prog_add_line_duplicate_label(void) {
    TEST("prog_add_line rejects duplicate label");
    prog_init();

    ASSERT_TRUE(prog_add_line("10 X = 1", 10));
    ASSERT_TRUE(!prog_add_line("10 Y = 2", 10));  /* Should fail */
    ASSERT_EQ(1, program.num_lines);
    TEST_PASS();
}

void test_prog_next_line(void) {
    TEST("prog_next_line advances");
    prog_init();

    prog_add_line("LINE 1", 0);
    prog_add_line("LINE 2", 0);
    prog_add_line("LINE 3", 0);

    ASSERT_EQ(0, program.current_line);
    prog_next_line();
    ASSERT_EQ(1, program.current_line);
    prog_next_line();
    ASSERT_EQ(2, program.current_line);
    TEST_PASS();
}

void test_prog_goto(void) {
    TEST("prog_goto jumps to label");
    prog_init();

    prog_add_line("X = 1", 0);
    prog_add_line("X = 2", 0);
    prog_add_line("100 X = 3", 100);

    ASSERT_TRUE(prog_goto(100));
    ASSERT_EQ(2, program.current_line);
    TEST_PASS();
}

void test_prog_goto_invalid(void) {
    TEST("prog_goto fails for invalid label");
    prog_init();

    prog_add_line("X = 1", 0);

    ASSERT_TRUE(!prog_goto(999));
    ASSERT_EQ(0, program.current_line);
    TEST_PASS();
}

/*============================================================================
 * Test: DO loop stack
 *============================================================================*/
void test_prog_push_do(void) {
    TEST("prog_push_do adds to stack");
    prog_init();
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    ASSERT_TRUE(prog_push_do(10, "I", 5, 1));
    ASSERT_EQ(1, program.do_depth);
    ASSERT_EQ(10, program.do_stack[0].end_label);
    ASSERT_STR_EQ("I", program.do_stack[0].var_name);
    TEST_PASS();
}

void test_prog_push_do_max_depth(void) {
    TEST("prog_push_do fails at max depth");
    prog_init();

    for (int i = 0; i < MAX_DO_NEST; i++) {
        ASSERT_TRUE(prog_push_do(i + 10, "I", 5, 1));
    }
    ASSERT_TRUE(!prog_push_do(100, "I", 5, 1));
    TEST_PASS();
}

void test_prog_pop_do_loop_continues(void) {
    TEST("prog_pop_do continues loop when not done");
    prog_init();
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    /* Set up: I = 1, loop to 3, step 1 */
    symbol_t *sym = sym_add("I", TYPE_INTEGER, SYM_VARIABLE, 1);
    sym_set_int(sym, 0, 1);

    prog_add_line("DO 10 I = 1, 3", 0);
    prog_add_line("X = I", 0);
    prog_add_line("10 CONTINUE", 10);

    program.current_line = 0;
    prog_push_do(10, "I", 3, 1);

    /* Simulate reaching CONTINUE at line 2 */
    program.current_line = 2;
    ASSERT_TRUE(prog_pop_do(10));

    /* Should have incremented I to 2 and set jump flag */
    ASSERT_EQ(2, sym_get_int(sym, 0));
    ASSERT_TRUE((program.do_depth & 0x80) != 0);  /* GOTO flag set */
    ASSERT_EQ(1, program.current_line);  /* Back to line after DO */
    TEST_PASS();
}

void test_prog_pop_do_loop_ends(void) {
    TEST("prog_pop_do ends loop when done");
    prog_init();
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    /* Set up: I = 3, loop to 3, step 1 - should end */
    symbol_t *sym = sym_add("I", TYPE_INTEGER, SYM_VARIABLE, 1);
    sym_set_int(sym, 0, 3);

    prog_add_line("DO 10 I = 1, 3", 0);
    prog_add_line("X = I", 0);
    prog_add_line("10 CONTINUE", 10);

    program.current_line = 0;
    prog_push_do(10, "I", 3, 1);

    /* Simulate reaching CONTINUE */
    program.current_line = 2;
    ASSERT_TRUE(prog_pop_do(10));

    /* I incremented to 4, which > 3, so loop ends */
    ASSERT_EQ(4, sym_get_int(sym, 0));
    ASSERT_EQ(0, program.do_depth);  /* Stack popped */
    TEST_PASS();
}

/*============================================================================
 * Test: IF block stack
 *============================================================================*/
void test_prog_push_if_true(void) {
    TEST("prog_push_if with true condition");
    prog_init();

    ASSERT_TRUE(prog_push_if(TRUE));
    ASSERT_EQ(1, program.if_depth);
    ASSERT_TRUE(program.if_stack[0].condition);
    ASSERT_TRUE(!program.if_stack[0].in_else);
    TEST_PASS();
}

void test_prog_push_if_false(void) {
    TEST("prog_push_if with false condition");
    prog_init();

    ASSERT_TRUE(prog_push_if(FALSE));
    ASSERT_EQ(1, program.if_depth);
    ASSERT_TRUE(!program.if_stack[0].condition);
    TEST_PASS();
}

void test_prog_push_if_max_depth(void) {
    TEST("prog_push_if fails at max depth");
    prog_init();

    for (int i = 0; i < MAX_IF_NEST; i++) {
        ASSERT_TRUE(prog_push_if(TRUE));
    }
    ASSERT_TRUE(!prog_push_if(TRUE));
    TEST_PASS();
}

void test_prog_pop_if(void) {
    TEST("prog_pop_if removes from stack");
    prog_init();

    prog_push_if(TRUE);
    prog_push_if(FALSE);
    ASSERT_EQ(2, program.if_depth);

    ASSERT_TRUE(prog_pop_if());
    ASSERT_EQ(1, program.if_depth);

    ASSERT_TRUE(prog_pop_if());
    ASSERT_EQ(0, program.if_depth);
    TEST_PASS();
}

void test_prog_pop_if_empty(void) {
    TEST("prog_pop_if fails when empty");
    prog_init();

    ASSERT_TRUE(!prog_pop_if());
    TEST_PASS();
}

void test_prog_handle_else_true_if(void) {
    TEST("prog_handle_else when IF was true");
    prog_init();

    prog_push_if(TRUE);

    /* ELSE should not execute when IF was true */
    bool_t should_execute = prog_handle_else();
    ASSERT_TRUE(!should_execute);
    ASSERT_TRUE(program.if_stack[0].in_else);
    TEST_PASS();
}

void test_prog_handle_else_false_if(void) {
    TEST("prog_handle_else when IF was false");
    prog_init();

    prog_push_if(FALSE);

    /* ELSE should execute when IF was false */
    bool_t should_execute = prog_handle_else();
    ASSERT_TRUE(should_execute);
    ASSERT_TRUE(program.if_stack[0].in_else);
    TEST_PASS();
}

/*============================================================================
 * Test: Subprogram management
 *============================================================================*/
void test_prog_add_subprog(void) {
    TEST("prog_add_subprog creates subprogram");
    prog_init();

    subprog_t *sp = prog_add_subprog("MYSUB", SUBPROG_SUBROUTINE, TYPE_INTEGER);

    ASSERT_TRUE(sp != NULL);
    ASSERT_STR_EQ("MYSUB", sp->name);
    ASSERT_EQ(SUBPROG_SUBROUTINE, sp->type);
    ASSERT_EQ(1, program.num_subprogs);
    TEST_PASS();
}

void test_prog_add_function(void) {
    TEST("prog_add_subprog creates function");
    prog_init();

    subprog_t *sp = prog_add_subprog("MYFUNC", SUBPROG_FUNCTION, TYPE_REAL);

    ASSERT_TRUE(sp != NULL);
    ASSERT_EQ(SUBPROG_FUNCTION, sp->type);
    ASSERT_EQ(TYPE_REAL, sp->return_type);
    TEST_PASS();
}

void test_prog_find_subprog(void) {
    TEST("prog_find_subprog finds by name");
    prog_init();

    prog_add_subprog("SUB1", SUBPROG_SUBROUTINE, TYPE_INTEGER);
    prog_add_subprog("SUB2", SUBPROG_SUBROUTINE, TYPE_INTEGER);

    subprog_t *sp = prog_find_subprog("SUB2");
    ASSERT_TRUE(sp != NULL);
    ASSERT_STR_EQ("SUB2", sp->name);
    TEST_PASS();
}

void test_prog_find_subprog_not_found(void) {
    TEST("prog_find_subprog returns NULL if not found");
    prog_init();

    ASSERT_TRUE(prog_find_subprog("NOSUCH") == NULL);
    TEST_PASS();
}

/*============================================================================
 * Test: Call stack
 *============================================================================*/
void test_prog_push_call(void) {
    TEST("prog_push_call adds frame");
    prog_init();

    ASSERT_TRUE(prog_push_call(5, 0));
    ASSERT_EQ(1, program.call_depth);
    ASSERT_EQ(5, program.call_stack[0].return_line);
    TEST_PASS();
}

void test_prog_push_call_max_depth(void) {
    TEST("prog_push_call fails at max depth");
    prog_init();

    for (int i = 0; i < MAX_CALL_DEPTH; i++) {
        ASSERT_TRUE(prog_push_call(i, 0));
    }
    ASSERT_TRUE(!prog_push_call(100, 0));
    TEST_PASS();
}

void test_prog_pop_call(void) {
    TEST("prog_pop_call restores state");
    prog_init();

    program.do_depth = 2;
    program.if_depth = 1;
    program.current_line = 10;

    prog_push_call(10, 0);

    /* Simulate being inside subprogram with different state */
    program.do_depth = 0;
    program.if_depth = 0;
    program.current_line = 20;

    ASSERT_TRUE(prog_pop_call());

    ASSERT_EQ(0, program.call_depth);
    ASSERT_EQ(10, program.current_line);
    /* do_depth has GOTO flag set */
    ASSERT_TRUE((program.do_depth & 0x80) != 0);
    TEST_PASS();
}

void test_prog_pop_call_empty(void) {
    TEST("prog_pop_call fails when empty");
    prog_init();

    ASSERT_TRUE(!prog_pop_call());
    TEST_PASS();
}

void test_prog_in_subprog(void) {
    TEST("prog_in_subprog returns correct state");
    prog_init();

    ASSERT_TRUE(!prog_in_subprog());

    prog_push_call(5, 0);
    ASSERT_TRUE(prog_in_subprog());

    prog_pop_call();
    ASSERT_TRUE(!prog_in_subprog());
    TEST_PASS();
}

/*============================================================================
 * Test: Program state
 *============================================================================*/
void test_prog_at_end(void) {
    TEST("prog_at_end checks end of program");
    prog_init();

    prog_add_line("X = 1", 0);
    prog_add_line("X = 2", 0);

    ASSERT_TRUE(!prog_at_end());

    program.current_line = 2;
    ASSERT_TRUE(prog_at_end());
    TEST_PASS();
}

void test_prog_at_end_stopped(void) {
    TEST("prog_at_end true when stopped");
    prog_init();

    prog_add_line("X = 1", 0);

    program.stopped = TRUE;
    ASSERT_TRUE(prog_at_end());
    TEST_PASS();
}

/*============================================================================
 * Main
 *============================================================================*/
int main(void) {
    printf("Program Unit Tests\n");
    printf("==================\n\n");

    /* Initialization */
    test_prog_init();

    /* Adding and retrieving lines */
    test_prog_add_line();
    test_prog_add_line_with_label();
    test_prog_add_line_duplicate_label();
    test_prog_next_line();
    test_prog_goto();
    test_prog_goto_invalid();

    /* DO loop stack */
    test_prog_push_do();
    test_prog_push_do_max_depth();
    test_prog_pop_do_loop_continues();
    test_prog_pop_do_loop_ends();

    /* IF block stack */
    test_prog_push_if_true();
    test_prog_push_if_false();
    test_prog_push_if_max_depth();
    test_prog_pop_if();
    test_prog_pop_if_empty();
    test_prog_handle_else_true_if();
    test_prog_handle_else_false_if();

    /* Subprogram management */
    test_prog_add_subprog();
    test_prog_add_function();
    test_prog_find_subprog();
    test_prog_find_subprog_not_found();

    /* Call stack */
    test_prog_push_call();
    test_prog_push_call_max_depth();
    test_prog_pop_call();
    test_prog_pop_call_empty();
    test_prog_in_subprog();

    /* Program state */
    test_prog_at_end();
    test_prog_at_end_stopped();

    TEST_SUMMARY();
    return TEST_EXIT_CODE();
}
