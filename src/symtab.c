/*
 * symtab.c - Symbol Table for kz80_fortran
 */

#include "symtab.h"
#include "bcd.h"
#include <string.h>

/*============================================================================
 * Global symbol table instance
 *============================================================================*/
symtab_t symtab;

/*============================================================================
 * Variable storage (in RAM at MEM_VARS)
 *============================================================================*/
static uint8_t *var_storage = (uint8_t *)MEM_VARS;

/*============================================================================
 * Helper: convert name to uppercase for comparison
 *============================================================================*/
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

/*============================================================================
 * Symbol table functions
 *============================================================================*/

void sym_init_all(void) {
    /* Clear counters and addresses */
    symtab.num_symbols = 0;
    symtab.next_addr = 0;
    symtab.num_labels = 0;
    symtab.num_common_blocks = 0;
}

void sym_init(void) {
    /* Clear only symbol entries, not labels */
    symtab.num_symbols = 0;
    symtab.next_addr = 0;

    /* Note: We don't clear variable storage here because it's
     * re-initialized as variables are declared during program execution.
     * The BCD print function now validates digit values to handle
     * any uninitialized memory gracefully. */
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

symbol_t *sym_add(const char *name, uint8_t type, uint8_t flags, uint16_t size) {
    if (symtab.num_symbols >= MAX_SYMBOLS) {
        return NULL;  /* Table full */
    }

    /* Check for duplicate */
    if (sym_lookup(name) != NULL) {
        return NULL;  /* Already exists */
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

    /* Calculate storage size */
    uint16_t bytes_per_elem;
    if (type == TYPE_INTEGER) {
        bytes_per_elem = 2;  /* 16-bit integer */
    } else if (type == TYPE_REAL) {
        bytes_per_elem = 6;  /* BCD real */
    } else {
        bytes_per_elem = 2;  /* Default */
    }

    symtab.next_addr += size * bytes_per_elem;
    symtab.num_symbols++;

    return sym;
}

symbol_t *sym_add_array(const char *name, uint8_t type, uint8_t flags,
                        uint8_t num_dims, const uint16_t *dims) {
    if (symtab.num_symbols >= MAX_SYMBOLS) {
        return NULL;  /* Table full */
    }

    /* Check for duplicate */
    if (sym_lookup(name) != NULL) {
        return NULL;  /* Already exists */
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

    /* Copy dimensions and compute total size */
    uint16_t total_size = 1;
    for (uint8_t i = 0; i < MAX_DIMS; i++) {
        if (i < num_dims && dims[i] > 0) {
            sym->dims[i] = dims[i];
            total_size *= dims[i];
        } else {
            sym->dims[i] = 1;
        }
    }

    /* Calculate storage size */
    uint16_t bytes_per_elem;
    if (type == TYPE_INTEGER) {
        bytes_per_elem = 2;  /* 16-bit integer */
    } else if (type == TYPE_REAL) {
        bytes_per_elem = 6;  /* BCD real */
    } else {
        bytes_per_elem = 2;  /* Default */
    }

    symtab.next_addr += total_size * bytes_per_elem;
    symtab.num_symbols++;

    return sym;
}

uint16_t sym_total_size(const symbol_t *sym) {
    if (sym == NULL) return 0;
    if (sym->num_dims == 0) return 1;  /* Scalar */

    uint16_t total = 1;
    for (uint8_t i = 0; i < sym->num_dims; i++) {
        total *= sym->dims[i];
    }
    return total;
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

/*============================================================================
 * Label table functions
 *============================================================================*/

void label_init(void) {
    symtab.num_labels = 0;
}

bool_t label_add(uint16_t label, uint16_t line, uint16_t addr) {
    /* Check for duplicate */
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
 * COMMON block functions
 *============================================================================*/

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
        return NULL;  /* Table full */
    }

    common_block_t *cb = &symtab.common_blocks[symtab.num_common_blocks];
    strcpy(cb->name, upper_name);

    /* Allocate space for this block from the variable storage */
    /* Each COMMON block gets up to 256 bytes */
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
        /* Already exists - if it's in the same COMMON block, that's OK */
        if (existing->flags & SYM_COMMON) {
            return existing;
        }
        return NULL;  /* Duplicate non-COMMON variable */
    }

    if (symtab.num_symbols >= MAX_SYMBOLS) {
        return NULL;  /* Table full */
    }

    symbol_t *sym = &symtab.symbols[symtab.num_symbols];

    to_upper_str(sym->name, name);
    sym->type = type;
    sym->flags = SYM_VARIABLE | SYM_COMMON;
    sym->addr = cb->base_addr + cb->next_offset;

    /* For backwards compatibility: 1D array with given size */
    if (size > 1) {
        sym->num_dims = 1;
        sym->dims[0] = size;
        sym->dims[1] = 1;
        sym->dims[2] = 1;
        sym->flags |= SYM_ARRAY;
    } else {
        sym->num_dims = 0;  /* Scalar */
        sym->dims[0] = 1;
        sym->dims[1] = 1;
        sym->dims[2] = 1;
    }

    /* Calculate storage size */
    uint16_t bytes_per_elem;
    if (type == TYPE_INTEGER) {
        bytes_per_elem = 2;  /* 16-bit integer */
    } else if (type == TYPE_REAL) {
        bytes_per_elem = 6;  /* BCD real */
    } else {
        bytes_per_elem = 2;  /* Default */
    }

    cb->next_offset += size * bytes_per_elem;

    /* Update global next_addr to account for COMMON storage if this is a new allocation */
    if (cb->base_addr + cb->next_offset > symtab.next_addr) {
        symtab.next_addr = cb->base_addr + cb->next_offset;
    }

    symtab.num_symbols++;

    return sym;
}
