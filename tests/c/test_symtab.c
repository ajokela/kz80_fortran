/*
 * test_symtab.c - Unit tests for symbol table
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

/* Provide mock variable storage for host testing.
 * We'll include a modified version of symtab.c that uses our mock storage.
 */
static uint8_t mock_var_storage[MEM_VARS_SIZE];

/*============================================================================
 * Include symtab.c inline with modifications for host testing
 *============================================================================*/
#include "../../include/symtab.h"

/* Global symbol table instance */
symtab_t symtab;

/* Variable storage - use our mock storage instead of MEM_VARS */
static uint8_t *var_storage = mock_var_storage;

/* Helper: convert name to uppercase for comparison */
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
    if (symtab.num_symbols >= MAX_SYMBOLS) {
        return NULL;
    }

    if (sym_lookup(name) != NULL) {
        return NULL;
    }

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

    uint16_t bytes_per_elem;
    if (type == TYPE_INTEGER) {
        bytes_per_elem = 2;
    } else if (type == TYPE_REAL) {
        bytes_per_elem = 6;
    } else {
        bytes_per_elem = 2;
    }

    symtab.next_addr += size * bytes_per_elem;
    symtab.num_symbols++;

    return sym;
}

symbol_t *sym_add_array(const char *name, uint8_t type, uint8_t flags,
                        uint8_t num_dims, const uint16_t *dims) {
    if (symtab.num_symbols >= MAX_SYMBOLS) {
        return NULL;
    }

    if (sym_lookup(name) != NULL) {
        return NULL;
    }

    if (num_dims > MAX_DIMS) {
        num_dims = MAX_DIMS;
    }

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

    uint16_t bytes_per_elem;
    if (type == TYPE_INTEGER) {
        bytes_per_elem = 2;
    } else if (type == TYPE_REAL) {
        bytes_per_elem = 6;
    } else {
        bytes_per_elem = 2;
    }

    symtab.next_addr += total_size * bytes_per_elem;
    symtab.num_symbols++;

    return sym;
}

int16_t sym_get_int(symbol_t *sym, uint16_t index) {
    if (sym == NULL || index >= sym_total_size(sym)) {
        return 0;
    }

    uint16_t addr = sym->addr + index * 2;
    uint8_t *ptr = var_storage + addr;

    return (int16_t)(ptr[0] | (ptr[1] << 8));
}

void sym_set_int(symbol_t *sym, uint16_t index, int16_t value) {
    if (sym == NULL || index >= sym_total_size(sym)) {
        return;
    }

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
    if (sym == NULL || index >= sym_total_size(sym) || value == NULL) {
        return;
    }

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

void label_init(void) {
    symtab.num_labels = 0;
}

bool_t label_add(uint16_t label, uint16_t line, uint16_t addr) {
    if (label_lookup(label) != NULL) {
        return FALSE;
    }

    if (symtab.num_labels >= MAX_LABELS) {
        return FALSE;
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
        if (symtab.labels[i].label == label) {
            return &symtab.labels[i];
        }
    }
    return NULL;
}

/*============================================================================
 * Test: Symbol table initialization
 *============================================================================*/
void test_sym_init_all(void) {
    TEST("sym_init_all clears everything");

    /* Add some data first */
    sym_add("TEST", TYPE_INTEGER, SYM_VARIABLE, 1);
    label_add(100, 5, 5);

    /* Init should clear it */
    sym_init_all();

    ASSERT_EQ(0, symtab.num_symbols);
    ASSERT_EQ(0, symtab.next_addr);
    ASSERT_EQ(0, symtab.num_labels);
    TEST_PASS();
}

void test_sym_init_keeps_labels(void) {
    TEST("sym_init clears symbols but keeps labels");

    sym_init_all();
    label_add(100, 5, 5);
    sym_add("TEST", TYPE_INTEGER, SYM_VARIABLE, 1);

    sym_init();

    ASSERT_EQ(0, symtab.num_symbols);
    ASSERT_EQ(1, symtab.num_labels);  /* Label should remain */
    TEST_PASS();
}

/*============================================================================
 * Test: Adding symbols
 *============================================================================*/
void test_sym_add_integer(void) {
    TEST("sym_add integer variable");
    sym_init_all();

    symbol_t *sym = sym_add("MYINT", TYPE_INTEGER, SYM_VARIABLE, 1);

    ASSERT_TRUE(sym != NULL);
    ASSERT_STR_EQ("MYINT", sym->name);
    ASSERT_EQ(TYPE_INTEGER, sym->type);
    ASSERT_EQ(SYM_VARIABLE, sym->flags);
    ASSERT_EQ(1, sym_total_size(sym));
    ASSERT_EQ(0, sym->addr);
    TEST_PASS();
}

void test_sym_add_real(void) {
    TEST("sym_add real variable");
    sym_init_all();

    symbol_t *sym = sym_add("MYREAL", TYPE_REAL, SYM_VARIABLE, 1);

    ASSERT_TRUE(sym != NULL);
    ASSERT_STR_EQ("MYREAL", sym->name);
    ASSERT_EQ(TYPE_REAL, sym->type);
    ASSERT_EQ(0, sym->addr);
    TEST_PASS();
}

void test_sym_add_array(void) {
    TEST("sym_add array variable");
    sym_init_all();

    symbol_t *sym = sym_add("ARR", TYPE_INTEGER, SYM_VARIABLE | SYM_ARRAY, 10);

    ASSERT_TRUE(sym != NULL);
    ASSERT_EQ(10, sym_total_size(sym));
    ASSERT_EQ(SYM_VARIABLE | SYM_ARRAY, sym->flags);
    TEST_PASS();
}

void test_sym_add_uppercase(void) {
    TEST("sym_add converts to uppercase");
    sym_init_all();

    symbol_t *sym = sym_add("lowercase", TYPE_INTEGER, SYM_VARIABLE, 1);

    ASSERT_TRUE(sym != NULL);
    ASSERT_STR_EQ("LOWERCASE", sym->name);
    TEST_PASS();
}

void test_sym_add_duplicate_fails(void) {
    TEST("sym_add rejects duplicate");
    sym_init_all();

    sym_add("DUP", TYPE_INTEGER, SYM_VARIABLE, 1);
    symbol_t *sym2 = sym_add("DUP", TYPE_REAL, SYM_VARIABLE, 1);

    ASSERT_TRUE(sym2 == NULL);
    ASSERT_EQ(1, symtab.num_symbols);
    TEST_PASS();
}

void test_sym_add_address_allocation(void) {
    TEST("sym_add allocates correct addresses");
    sym_init_all();

    symbol_t *s1 = sym_add("INT1", TYPE_INTEGER, SYM_VARIABLE, 1);  /* 2 bytes */
    symbol_t *s2 = sym_add("INT2", TYPE_INTEGER, SYM_VARIABLE, 1);  /* 2 bytes */
    symbol_t *s3 = sym_add("REAL1", TYPE_REAL, SYM_VARIABLE, 1);    /* 6 bytes */

    ASSERT_EQ(0, s1->addr);
    ASSERT_EQ(2, s2->addr);
    ASSERT_EQ(4, s3->addr);
    ASSERT_EQ(10, symtab.next_addr);  /* 2 + 2 + 6 */
    TEST_PASS();
}

void test_sym_add_array_address(void) {
    TEST("sym_add allocates array storage");
    sym_init_all();

    symbol_t *arr = sym_add("ARR", TYPE_INTEGER, SYM_VARIABLE | SYM_ARRAY, 5);
    symbol_t *next = sym_add("NEXT", TYPE_INTEGER, SYM_VARIABLE, 1);

    ASSERT_EQ(0, arr->addr);
    ASSERT_EQ(10, next->addr);  /* 5 * 2 bytes */
    TEST_PASS();
}

/*============================================================================
 * Test: Looking up symbols
 *============================================================================*/
void test_sym_lookup_found(void) {
    TEST("sym_lookup finds existing symbol");
    sym_init_all();

    sym_add("FINDME", TYPE_INTEGER, SYM_VARIABLE, 1);

    symbol_t *sym = sym_lookup("FINDME");

    ASSERT_TRUE(sym != NULL);
    ASSERT_STR_EQ("FINDME", sym->name);
    TEST_PASS();
}

void test_sym_lookup_not_found(void) {
    TEST("sym_lookup returns NULL for missing");
    sym_init_all();

    symbol_t *sym = sym_lookup("NOTEXIST");

    ASSERT_TRUE(sym == NULL);
    TEST_PASS();
}

void test_sym_lookup_case_insensitive(void) {
    TEST("sym_lookup is case insensitive");
    sym_init_all();

    sym_add("MYVAR", TYPE_INTEGER, SYM_VARIABLE, 1);

    ASSERT_TRUE(sym_lookup("MYVAR") != NULL);
    ASSERT_TRUE(sym_lookup("myvar") != NULL);
    ASSERT_TRUE(sym_lookup("MyVar") != NULL);
    TEST_PASS();
}

/*============================================================================
 * Test: Integer get/set
 *============================================================================*/
void test_sym_set_get_int(void) {
    TEST("sym_set_int and sym_get_int");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    symbol_t *sym = sym_add("X", TYPE_INTEGER, SYM_VARIABLE, 1);

    sym_set_int(sym, 0, 12345);
    int16_t val = sym_get_int(sym, 0);

    ASSERT_EQ(12345, val);
    ASSERT_TRUE((sym->flags & SYM_DEFINED) != 0);
    TEST_PASS();
}

void test_sym_int_negative(void) {
    TEST("sym integer negative value");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    symbol_t *sym = sym_add("NEG", TYPE_INTEGER, SYM_VARIABLE, 1);

    sym_set_int(sym, 0, -500);
    int16_t val = sym_get_int(sym, 0);

    ASSERT_EQ(-500, val);
    TEST_PASS();
}

void test_sym_int_array(void) {
    TEST("sym integer array access");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    symbol_t *arr = sym_add("ARR", TYPE_INTEGER, SYM_VARIABLE | SYM_ARRAY, 5);

    sym_set_int(arr, 0, 100);
    sym_set_int(arr, 2, 300);
    sym_set_int(arr, 4, 500);

    ASSERT_EQ(100, sym_get_int(arr, 0));
    ASSERT_EQ(0, sym_get_int(arr, 1));    /* Unset element */
    ASSERT_EQ(300, sym_get_int(arr, 2));
    ASSERT_EQ(0, sym_get_int(arr, 3));
    ASSERT_EQ(500, sym_get_int(arr, 4));
    TEST_PASS();
}

void test_sym_int_bounds_check(void) {
    TEST("sym_get_int returns 0 for out of bounds");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    symbol_t *sym = sym_add("X", TYPE_INTEGER, SYM_VARIABLE, 1);
    sym_set_int(sym, 0, 42);

    ASSERT_EQ(0, sym_get_int(sym, 1));   /* Out of bounds */
    ASSERT_EQ(0, sym_get_int(sym, 100)); /* Way out of bounds */
    ASSERT_EQ(0, sym_get_int(NULL, 0));  /* NULL symbol */
    TEST_PASS();
}

/*============================================================================
 * Test: Real get/set
 *============================================================================*/
void test_sym_set_get_real(void) {
    TEST("sym_set_real and sym_get_real");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    symbol_t *sym = sym_add("R", TYPE_REAL, SYM_VARIABLE, 1);

    bcd_t val_in, val_out;
    bcd_from_int(&val_in, 314);
    val_in.exp = BCD_EXP_BIAS + 1;  /* 3.14 */

    sym_set_real(sym, 0, &val_in);
    sym_get_real(sym, 0, &val_out);

    ASSERT_EQ(val_in.sign, val_out.sign);
    ASSERT_EQ(val_in.exp, val_out.exp);
    ASSERT_EQ(val_in.digits[0], val_out.digits[0]);
    ASSERT_TRUE((sym->flags & SYM_DEFINED) != 0);
    TEST_PASS();
}

void test_sym_real_array(void) {
    TEST("sym real array access");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    symbol_t *arr = sym_add("RARR", TYPE_REAL, SYM_VARIABLE | SYM_ARRAY, 3);

    bcd_t v0, v1, v2, out;
    bcd_from_int(&v0, 100);
    bcd_from_int(&v1, 200);
    bcd_from_int(&v2, 300);

    sym_set_real(arr, 0, &v0);
    sym_set_real(arr, 1, &v1);
    sym_set_real(arr, 2, &v2);

    sym_get_real(arr, 0, &out);
    ASSERT_EQ(100, bcd_to_int(&out));

    sym_get_real(arr, 1, &out);
    ASSERT_EQ(200, bcd_to_int(&out));

    sym_get_real(arr, 2, &out);
    ASSERT_EQ(300, bcd_to_int(&out));
    TEST_PASS();
}

void test_sym_real_bounds_check(void) {
    TEST("sym_get_real returns 0 for out of bounds");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    symbol_t *sym = sym_add("R", TYPE_REAL, SYM_VARIABLE, 1);
    bcd_t val;
    bcd_from_int(&val, 42);
    sym_set_real(sym, 0, &val);

    bcd_t out;
    sym_get_real(sym, 1, &out);  /* Out of bounds */
    ASSERT_EQ(0, bcd_to_int(&out));

    sym_get_real(NULL, 0, &out); /* NULL symbol */
    ASSERT_EQ(0, bcd_to_int(&out));
    TEST_PASS();
}

/*============================================================================
 * Test: Label table
 *============================================================================*/
void test_label_add(void) {
    TEST("label_add creates label");
    sym_init_all();

    bool_t result = label_add(100, 5, 50);

    ASSERT_TRUE(result);
    ASSERT_EQ(1, symtab.num_labels);
    TEST_PASS();
}

void test_label_lookup_found(void) {
    TEST("label_lookup finds existing label");
    sym_init_all();

    label_add(100, 5, 50);
    label_add(200, 10, 100);

    label_t *lbl = label_lookup(100);

    ASSERT_TRUE(lbl != NULL);
    ASSERT_EQ(100, lbl->label);
    ASSERT_EQ(5, lbl->line);
    ASSERT_EQ(50, lbl->addr);
    TEST_PASS();
}

void test_label_lookup_not_found(void) {
    TEST("label_lookup returns NULL for missing");
    sym_init_all();

    label_add(100, 5, 50);

    label_t *lbl = label_lookup(999);

    ASSERT_TRUE(lbl == NULL);
    TEST_PASS();
}

void test_label_duplicate_fails(void) {
    TEST("label_add rejects duplicate");
    sym_init_all();

    label_add(100, 5, 50);
    bool_t result = label_add(100, 10, 100);  /* Same label number */

    ASSERT_TRUE(!result);
    ASSERT_EQ(1, symtab.num_labels);
    TEST_PASS();
}

void test_label_init(void) {
    TEST("label_init clears labels");
    sym_init_all();

    label_add(100, 5, 50);
    label_add(200, 10, 100);

    label_init();

    ASSERT_EQ(0, symtab.num_labels);
    TEST_PASS();
}

/*============================================================================
 * Test: Multi-dimensional arrays
 *============================================================================*/
void test_sym_add_2d_array(void) {
    TEST("sym_add_array creates 2D array");
    sym_init_all();

    uint16_t dims[2] = {3, 4};  /* 3x4 array */
    symbol_t *sym = sym_add_array("MATRIX", TYPE_INTEGER, 0, 2, dims);

    ASSERT_TRUE(sym != NULL);
    ASSERT_STR_EQ("MATRIX", sym->name);
    ASSERT_EQ(TYPE_INTEGER, sym->type);
    ASSERT_EQ(2, sym->num_dims);
    ASSERT_EQ(3, sym->dims[0]);
    ASSERT_EQ(4, sym->dims[1]);
    ASSERT_EQ(12, sym_total_size(sym));  /* 3*4 = 12 elements */
    ASSERT_TRUE((sym->flags & SYM_ARRAY) != 0);
    TEST_PASS();
}

void test_sym_add_3d_array(void) {
    TEST("sym_add_array creates 3D array");
    sym_init_all();

    uint16_t dims[3] = {2, 3, 4};  /* 2x3x4 array */
    symbol_t *sym = sym_add_array("CUBE", TYPE_REAL, 0, 3, dims);

    ASSERT_TRUE(sym != NULL);
    ASSERT_EQ(3, sym->num_dims);
    ASSERT_EQ(2, sym->dims[0]);
    ASSERT_EQ(3, sym->dims[1]);
    ASSERT_EQ(4, sym->dims[2]);
    ASSERT_EQ(24, sym_total_size(sym));  /* 2*3*4 = 24 elements */
    TEST_PASS();
}

void test_sym_2d_array_storage(void) {
    TEST("sym 2D array stores values correctly");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    uint16_t dims[2] = {3, 4};  /* 3x4 array */
    symbol_t *arr = sym_add_array("MAT", TYPE_INTEGER, 0, 2, dims);

    /* Set values at different positions using linear indices */
    /* Row-major: (i,j) -> i*4 + j for dims (3,4) */
    sym_set_int(arr, 0, 100);     /* (0,0) */
    sym_set_int(arr, 5, 250);     /* (1,1): 1*4 + 1 = 5 */
    sym_set_int(arr, 11, 400);    /* (2,3): 2*4 + 3 = 11 */

    ASSERT_EQ(100, sym_get_int(arr, 0));
    ASSERT_EQ(250, sym_get_int(arr, 5));
    ASSERT_EQ(400, sym_get_int(arr, 11));
    ASSERT_EQ(0, sym_get_int(arr, 6));  /* Unset position */
    TEST_PASS();
}

void test_sym_3d_array_storage_allocation(void) {
    TEST("sym 3D array allocates correct storage");
    sym_init_all();

    uint16_t dims[3] = {2, 3, 4};  /* 2x3x4 = 24 elements */
    symbol_t *arr = sym_add_array("CUBE", TYPE_INTEGER, 0, 3, dims);
    symbol_t *next = sym_add("X", TYPE_INTEGER, SYM_VARIABLE, 1);

    /* 24 integers * 2 bytes = 48 bytes */
    ASSERT_EQ(0, arr->addr);
    ASSERT_EQ(48, next->addr);
    TEST_PASS();
}

void test_sym_2d_real_array(void) {
    TEST("sym 2D real array works correctly");
    sym_init_all();
    memset(mock_var_storage, 0, sizeof(mock_var_storage));

    uint16_t dims[2] = {2, 2};  /* 2x2 array */
    symbol_t *arr = sym_add_array("RMAT", TYPE_REAL, 0, 2, dims);

    bcd_t v1, v2, v3, out;
    bcd_from_int(&v1, 10);
    bcd_from_int(&v2, 20);
    bcd_from_int(&v3, 30);

    sym_set_real(arr, 0, &v1);  /* (0,0) */
    sym_set_real(arr, 3, &v2);  /* (1,1) */

    sym_get_real(arr, 0, &out);
    ASSERT_EQ(10, bcd_to_int(&out));

    sym_get_real(arr, 3, &out);
    ASSERT_EQ(20, bcd_to_int(&out));

    /* Check unset position returns 0 */
    sym_get_real(arr, 1, &out);
    ASSERT_EQ(0, bcd_to_int(&out));
    TEST_PASS();
}

/*============================================================================
 * Main
 *============================================================================*/
int main(void) {
    printf("Symbol Table Unit Tests\n");
    printf("=======================\n\n");

    /* Initialization */
    test_sym_init_all();
    test_sym_init_keeps_labels();

    /* Adding symbols */
    test_sym_add_integer();
    test_sym_add_real();
    test_sym_add_array();
    test_sym_add_uppercase();
    test_sym_add_duplicate_fails();
    test_sym_add_address_allocation();
    test_sym_add_array_address();

    /* Looking up symbols */
    test_sym_lookup_found();
    test_sym_lookup_not_found();
    test_sym_lookup_case_insensitive();

    /* Integer get/set */
    test_sym_set_get_int();
    test_sym_int_negative();
    test_sym_int_array();
    test_sym_int_bounds_check();

    /* Real get/set */
    test_sym_set_get_real();
    test_sym_real_array();
    test_sym_real_bounds_check();

    /* Labels */
    test_label_add();
    test_label_lookup_found();
    test_label_lookup_not_found();
    test_label_duplicate_fails();
    test_label_init();

    /* Multi-dimensional arrays */
    test_sym_add_2d_array();
    test_sym_add_3d_array();
    test_sym_2d_array_storage();
    test_sym_3d_array_storage_allocation();
    test_sym_2d_real_array();

    TEST_SUMMARY();
    return TEST_EXIT_CODE();
}
