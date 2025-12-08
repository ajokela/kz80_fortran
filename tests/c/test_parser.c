/*
 * test_parser.c - Unit tests for expression parser
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include "test_framework.h"
#include "mock_acia.h"

/* Include config and types first */
#include "../../include/config.h"
#include "../../include/types.h"

/* Need BCD for real values */
#include "../../src/bcd.c"

/* Mock variable storage for symtab */
static uint8_t mock_var_storage[MEM_VARS_SIZE];

/* Include symtab inline (same approach as test_symtab.c) */
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

    /* Look for existing block */
    for (uint8_t i = 0; i < symtab.num_common_blocks; i++) {
        if (strcmp(symtab.common_blocks[i].name, upper_name) == 0) {
            return &symtab.common_blocks[i];
        }
    }

    /* Create new block */
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

    /* Check for duplicate */
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

/* Mock program storage */
#include "../../include/program.h"
program_t program;
static prog_line_t mock_prog_lines[MAX_PROG_LINES];
#undef prog_lines
#define prog_lines mock_prog_lines

/* Stub program functions needed by parser */
void prog_init(void) {
    program.num_lines = 0;
    program.current_line = 0;
    program.running = FALSE;
    program.stopped = FALSE;
    program.do_depth = 0;
    program.if_depth = 0;
    program.num_subprogs = 0;
    program.call_depth = 0;
}

subprog_t *prog_find_subprog(const char *name) {
    for (uint8_t i = 0; i < program.num_subprogs; i++) {
        if (strcmp(program.subprogs[i].name, name) == 0) {
            return &program.subprogs[i];
        }
    }
    return NULL;
}

bool_t prog_in_subprog(void) { return program.call_depth > 0; }

/* Additional stubs needed by parser */
subprog_t *prog_add_subprog(const char *name, uint8_t type, uint8_t return_type) {
    if (program.num_subprogs >= MAX_SUBPROGRAMS) return NULL;
    subprog_t *sp = &program.subprogs[program.num_subprogs];
    strncpy(sp->name, name, MAX_IDENT_LEN);
    sp->name[MAX_IDENT_LEN] = '\0';
    sp->type = type;
    sp->return_type = return_type;
    sp->start_line = program.current_line + 1;
    sp->num_params = 0;
    program.num_subprogs++;
    return sp;
}

int8_t prog_find_label(uint16_t label) {
    label_t *lbl = label_lookup(label);
    if (lbl != NULL) return (int8_t)lbl->line;
    return -1;
}

const char *prog_get_line(void) {
    if (program.current_line < program.num_lines) {
        return mock_prog_lines[program.current_line].text;
    }
    return NULL;
}

void prog_next_line(void) {
    if (program.current_line < program.num_lines) {
        program.current_line++;
    }
}

bool_t prog_push_do(uint16_t end_label, const char *var_name,
                    int16_t end_val, int16_t step_val) {
    if (program.do_depth >= MAX_DO_NEST) return FALSE;
    do_state_t *ds = &program.do_stack[program.do_depth];
    ds->end_label = end_label;
    ds->line_idx = program.current_line;
    strncpy(ds->var_name, var_name, MAX_IDENT_LEN);
    ds->end_val = end_val;
    ds->step_val = step_val;
    program.do_depth++;
    return TRUE;
}

bool_t prog_pop_do(uint16_t label) {
    (void)label;
    if (program.do_depth > 0) {
        program.do_depth--;
        return TRUE;
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
    if (program.if_depth > 0) {
        program.if_depth--;
        return TRUE;
    }
    return FALSE;
}

bool_t prog_handle_else(void) {
    if (program.if_depth == 0) return FALSE;
    return TRUE;
}

void prog_skip_to_else_or_endif(void) { }
void prog_skip_subprog(void) { }

bool_t prog_push_call(uint8_t return_line, uint8_t subprog_idx) {
    if (program.call_depth >= MAX_CALL_DEPTH) return FALSE;
    call_frame_t *frame = &program.call_stack[program.call_depth];
    frame->return_line = return_line;
    frame->subprog_idx = subprog_idx;
    program.call_depth++;
    return TRUE;
}

bool_t prog_pop_call(void) {
    if (program.call_depth > 0) {
        program.call_depth--;
        return TRUE;
    }
    return FALSE;
}

/* Mock acia_gets for READ statement */
uint8_t acia_gets(char *buf, uint8_t max_len) {
    (void)buf;
    (void)max_len;
    buf[0] = '\0';
    return 0;
}

/* Include lexer */
#include "../../src/lexer.c"

/* Include parser */
#include "../../src/parser.c"

/*============================================================================
 * Helper to evaluate an expression and return result
 *============================================================================*/
static bool_t eval_int(const char *expr, int16_t *result) {
    parser_t p;
    parser_init(&p, expr, 1);
    expr_result_t r;
    if (!parse_expression(&p, &r)) return FALSE;
    real_to_int(&r);
    *result = r.val.i;
    return TRUE;
}

static bool_t eval_real(const char *expr, bcd_t *result) {
    parser_t p;
    parser_init(&p, expr, 1);
    expr_result_t r;
    if (!parse_expression(&p, &r)) return FALSE;
    int_to_real(&r);
    bcd_copy(result, &r.val.r);
    return TRUE;
}

/*============================================================================
 * Test: Integer literals
 *============================================================================*/
void test_parse_int_literal(void) {
    TEST("parse integer literal 42");
    int16_t result;
    ASSERT_TRUE(eval_int("42", &result));
    ASSERT_EQ(42, result);
    TEST_PASS();
}

void test_parse_negative_int(void) {
    TEST("parse negative integer -123");
    int16_t result;
    ASSERT_TRUE(eval_int("-123", &result));
    ASSERT_EQ(-123, result);
    TEST_PASS();
}

void test_parse_positive_int(void) {
    TEST("parse positive integer +456");
    int16_t result;
    ASSERT_TRUE(eval_int("+456", &result));
    ASSERT_EQ(456, result);
    TEST_PASS();
}

/*============================================================================
 * Test: Real literals
 *============================================================================*/
void test_parse_real_literal(void) {
    TEST("parse real literal 3.14");
    bcd_t result;
    ASSERT_TRUE(eval_real("3.14", &result));
    /* Check first digit is 3 */
    ASSERT_EQ(3, bcd_get_digit(&result, 0));
    TEST_PASS();
}

/*============================================================================
 * Test: Arithmetic operators
 *============================================================================*/
void test_parse_add(void) {
    TEST("parse 3 + 4 = 7");
    int16_t result;
    ASSERT_TRUE(eval_int("3 + 4", &result));
    ASSERT_EQ(7, result);
    TEST_PASS();
}

void test_parse_sub(void) {
    TEST("parse 10 - 3 = 7");
    int16_t result;
    ASSERT_TRUE(eval_int("10 - 3", &result));
    ASSERT_EQ(7, result);
    TEST_PASS();
}

void test_parse_mul(void) {
    TEST("parse 6 * 7 = 42");
    int16_t result;
    ASSERT_TRUE(eval_int("6 * 7", &result));
    ASSERT_EQ(42, result);
    TEST_PASS();
}

void test_parse_div(void) {
    TEST("parse 20 / 4 = 5");
    int16_t result;
    ASSERT_TRUE(eval_int("20 / 4", &result));
    ASSERT_EQ(5, result);
    TEST_PASS();
}

void test_parse_power(void) {
    TEST("parse 2 ** 3 = 8");
    int16_t result;
    ASSERT_TRUE(eval_int("2 ** 3", &result));
    ASSERT_EQ(8, result);
    TEST_PASS();
}

/*============================================================================
 * Test: Operator precedence
 *============================================================================*/
void test_parse_precedence_mul_add(void) {
    TEST("parse 2 + 3 * 4 = 14 (mul before add)");
    int16_t result;
    ASSERT_TRUE(eval_int("2 + 3 * 4", &result));
    ASSERT_EQ(14, result);
    TEST_PASS();
}

void test_parse_precedence_power_mul(void) {
    TEST("parse 2 * 3 ** 2 = 18 (power before mul)");
    int16_t result;
    ASSERT_TRUE(eval_int("2 * 3 ** 2", &result));
    ASSERT_EQ(18, result);
    TEST_PASS();
}

void test_parse_parentheses(void) {
    TEST("parse (2 + 3) * 4 = 20");
    int16_t result;
    ASSERT_TRUE(eval_int("(2 + 3) * 4", &result));
    ASSERT_EQ(20, result);
    TEST_PASS();
}

void test_parse_nested_parens(void) {
    TEST("parse ((1 + 2) * (3 + 4)) = 21");
    int16_t result;
    ASSERT_TRUE(eval_int("((1 + 2) * (3 + 4))", &result));
    ASSERT_EQ(21, result);
    TEST_PASS();
}

/*============================================================================
 * Test: Relational operators
 *============================================================================*/
void test_parse_eq_true(void) {
    TEST("parse 5 .EQ. 5 = 1 (true)");
    int16_t result;
    ASSERT_TRUE(eval_int("5 .EQ. 5", &result));
    ASSERT_EQ(1, result);
    TEST_PASS();
}

void test_parse_eq_false(void) {
    TEST("parse 5 .EQ. 6 = 0 (false)");
    int16_t result;
    ASSERT_TRUE(eval_int("5 .EQ. 6", &result));
    ASSERT_EQ(0, result);
    TEST_PASS();
}

void test_parse_ne(void) {
    TEST("parse 5 .NE. 6 = 1");
    int16_t result;
    ASSERT_TRUE(eval_int("5 .NE. 6", &result));
    ASSERT_EQ(1, result);
    TEST_PASS();
}

void test_parse_lt(void) {
    TEST("parse 3 .LT. 5 = 1");
    int16_t result;
    ASSERT_TRUE(eval_int("3 .LT. 5", &result));
    ASSERT_EQ(1, result);
    TEST_PASS();
}

void test_parse_gt(void) {
    TEST("parse 7 .GT. 5 = 1");
    int16_t result;
    ASSERT_TRUE(eval_int("7 .GT. 5", &result));
    ASSERT_EQ(1, result);
    TEST_PASS();
}

void test_parse_le(void) {
    TEST("parse 5 .LE. 5 = 1");
    int16_t result;
    ASSERT_TRUE(eval_int("5 .LE. 5", &result));
    ASSERT_EQ(1, result);
    TEST_PASS();
}

void test_parse_ge(void) {
    TEST("parse 5 .GE. 3 = 1");
    int16_t result;
    ASSERT_TRUE(eval_int("5 .GE. 3", &result));
    ASSERT_EQ(1, result);
    TEST_PASS();
}

/*============================================================================
 * Test: Logical operators
 *============================================================================*/
void test_parse_and_true(void) {
    TEST("parse 1 .AND. 1 = 1");
    int16_t result;
    ASSERT_TRUE(eval_int("1 .AND. 1", &result));
    ASSERT_EQ(1, result);
    TEST_PASS();
}

void test_parse_and_false(void) {
    TEST("parse 1 .AND. 0 = 0");
    int16_t result;
    ASSERT_TRUE(eval_int("1 .AND. 0", &result));
    ASSERT_EQ(0, result);
    TEST_PASS();
}

void test_parse_or_true(void) {
    TEST("parse 0 .OR. 1 = 1");
    int16_t result;
    ASSERT_TRUE(eval_int("0 .OR. 1", &result));
    ASSERT_EQ(1, result);
    TEST_PASS();
}

void test_parse_or_false(void) {
    TEST("parse 0 .OR. 0 = 0");
    int16_t result;
    ASSERT_TRUE(eval_int("0 .OR. 0", &result));
    ASSERT_EQ(0, result);
    TEST_PASS();
}

void test_parse_not_true(void) {
    TEST("parse .NOT. 0 = 1");
    int16_t result;
    ASSERT_TRUE(eval_int(".NOT. 0", &result));
    ASSERT_EQ(1, result);
    TEST_PASS();
}

void test_parse_not_false(void) {
    TEST("parse .NOT. 1 = 0");
    int16_t result;
    ASSERT_TRUE(eval_int(".NOT. 1", &result));
    ASSERT_EQ(0, result);
    TEST_PASS();
}

void test_parse_true_false(void) {
    TEST("parse .TRUE. .AND. .FALSE. = 0");
    int16_t result;
    ASSERT_TRUE(eval_int(".TRUE. .AND. .FALSE.", &result));
    ASSERT_EQ(0, result);
    TEST_PASS();
}

/*============================================================================
 * Test: Variables
 *============================================================================*/
void test_parse_int_variable(void) {
    TEST("parse integer variable X = 42");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    symbol_t *sym = sym_add("X", TYPE_INTEGER, SYM_VARIABLE, 1);
    sym_set_int(sym, 0, 42);

    int16_t result;
    ASSERT_TRUE(eval_int("X", &result));
    ASSERT_EQ(42, result);
    TEST_PASS();
}

void test_parse_real_variable(void) {
    TEST("parse real variable Y = 3.14");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    symbol_t *sym = sym_add("Y", TYPE_REAL, SYM_VARIABLE, 1);
    bcd_t val;
    bcd_parse(&val, "3.14");
    sym_set_real(sym, 0, &val);

    bcd_t result;
    ASSERT_TRUE(eval_real("Y", &result));
    ASSERT_EQ(3, bcd_get_digit(&result, 0));
    TEST_PASS();
}

void test_parse_variable_expr(void) {
    TEST("parse X + Y with X=10, Y=5");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    symbol_t *x = sym_add("X", TYPE_INTEGER, SYM_VARIABLE, 1);
    symbol_t *y = sym_add("Y", TYPE_INTEGER, SYM_VARIABLE, 1);
    sym_set_int(x, 0, 10);
    sym_set_int(y, 0, 5);

    int16_t result;
    ASSERT_TRUE(eval_int("X + Y", &result));
    ASSERT_EQ(15, result);
    TEST_PASS();
}

/*============================================================================
 * Test: Arrays
 *============================================================================*/
void test_parse_array_element(void) {
    TEST("parse array element ARR(3)");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    symbol_t *arr = sym_add("ARR", TYPE_INTEGER, SYM_VARIABLE | SYM_ARRAY, 10);
    sym_set_int(arr, 2, 99);  /* ARR(3) in Fortran = index 2 in C (1-based) */

    int16_t result;
    ASSERT_TRUE(eval_int("ARR(3)", &result));
    ASSERT_EQ(99, result);
    TEST_PASS();
}

/*============================================================================
 * Test: Intrinsic functions
 *============================================================================*/
#ifdef FEATURE_INTRINSICS
void test_parse_abs_positive(void) {
    TEST("parse ABS(5) = 5");
    int16_t result;
    ASSERT_TRUE(eval_int("ABS(5)", &result));
    ASSERT_EQ(5, result);
    TEST_PASS();
}

void test_parse_abs_negative(void) {
    TEST("parse ABS(-5) = 5");
    int16_t result;
    ASSERT_TRUE(eval_int("ABS(-5)", &result));
    ASSERT_EQ(5, result);
    TEST_PASS();
}

void test_parse_mod(void) {
    TEST("parse MOD(17, 5) = 2");
    int16_t result;
    ASSERT_TRUE(eval_int("MOD(17, 5)", &result));
    ASSERT_EQ(2, result);
    TEST_PASS();
}

void test_parse_int_func(void) {
    TEST("parse INT(3.7) = 3");
    bcd_t val;
    bcd_parse(&val, "3.7");

    /* We'll test via evaluation */
    parser_t p;
    parser_init(&p, "INT(3.7)", 1);
    expr_result_t r;
    ASSERT_TRUE(parse_expression(&p, &r));
    ASSERT_EQ(TYPE_INTEGER, r.type);
    ASSERT_EQ(3, r.val.i);
    TEST_PASS();
}
#endif /* FEATURE_INTRINSICS */

void test_parse_real_func(void) {
    TEST("parse REAL(42)");
    parser_t p;
    parser_init(&p, "REAL(42)", 1);
    expr_result_t r;
    ASSERT_TRUE(parse_expression(&p, &r));
    ASSERT_EQ(TYPE_REAL, r.type);
    ASSERT_EQ(42, bcd_to_int(&r.val.r));
    TEST_PASS();
}

/*============================================================================
 * Test: Complex expressions
 *============================================================================*/
void test_parse_complex_expr(void) {
    TEST("parse (3 + 4) * 2 - 10 / 5 = 12");
    int16_t result;
    ASSERT_TRUE(eval_int("(3 + 4) * 2 - 10 / 5", &result));
    ASSERT_EQ(12, result);
    TEST_PASS();
}

void test_parse_logical_expr(void) {
    TEST("parse (5 .GT. 3) .AND. (2 .LT. 4) = 1");
    int16_t result;
    ASSERT_TRUE(eval_int("(5 .GT. 3) .AND. (2 .LT. 4)", &result));
    ASSERT_EQ(1, result);
    TEST_PASS();
}

/*============================================================================
 * Test: Error handling
 *============================================================================*/
void test_parse_missing_paren(void) {
    TEST("parse (3 + 4 fails (missing paren)");
    parser_t p;
    parser_init(&p, "(3 + 4", 1);
    expr_result_t r;
    ASSERT_TRUE(!parse_expression(&p, &r));
    TEST_PASS();
}

void test_parse_undefined_var(void) {
    TEST("parse undefined variable uses implicit declaration");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));
    parser_t p;
    parser_init(&p, "UNDEFINED", 1);
    expr_result_t r;
    /* Fortran has implicit declaration - undefined vars are auto-declared */
    /* The parser creates implicit variables, so this should succeed */
    /* However, since we're not running, the variable won't be created */
    /* The parser should fail when not in running mode for undefined vars */
    bool_t result = parse_expression(&p, &r);
    /* In Fortran, variables can be used without explicit declaration */
    /* The parser handles this by returning 0 for undefined scalars */
    ASSERT_TRUE(result);  /* Expression parses - implicit declaration */
    TEST_PASS();
}

/*============================================================================
 * Main
 *============================================================================*/
int main(void) {
    printf("Parser Unit Tests\n");
    printf("=================\n\n");

    prog_init();

    /* Integer literals */
    test_parse_int_literal();
    test_parse_negative_int();
    test_parse_positive_int();

    /* Real literals */
    test_parse_real_literal();

    /* Arithmetic operators */
    test_parse_add();
    test_parse_sub();
    test_parse_mul();
    test_parse_div();
    test_parse_power();

    /* Operator precedence */
    test_parse_precedence_mul_add();
    test_parse_precedence_power_mul();
    test_parse_parentheses();
    test_parse_nested_parens();

    /* Relational operators */
    test_parse_eq_true();
    test_parse_eq_false();
    test_parse_ne();
    test_parse_lt();
    test_parse_gt();
    test_parse_le();
    test_parse_ge();

    /* Logical operators */
    test_parse_and_true();
    test_parse_and_false();
    test_parse_or_true();
    test_parse_or_false();
    test_parse_not_true();
    test_parse_not_false();
    test_parse_true_false();

    /* Variables */
    test_parse_int_variable();
    test_parse_real_variable();
    test_parse_variable_expr();

    /* Arrays */
    test_parse_array_element();

    /* Intrinsic functions */
#ifdef FEATURE_INTRINSICS
    test_parse_abs_positive();
    test_parse_abs_negative();
    test_parse_mod();
    test_parse_int_func();
#endif
    test_parse_real_func();  /* REAL() is always available */

    /* Complex expressions */
    test_parse_complex_expr();
    test_parse_logical_expr();

    /* Error handling */
    test_parse_missing_paren();
    test_parse_undefined_var();

    TEST_SUMMARY();
    return TEST_EXIT_CODE();
}
