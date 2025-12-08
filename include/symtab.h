/*
 * symtab.h - Symbol Table for kz80_fortran
 *
 * Manages variables, arrays, and statement labels.
 */

#ifndef SYMTAB_H
#define SYMTAB_H

#include "types.h"

/*============================================================================
 * Symbol flags
 *============================================================================*/
#define SYM_VARIABLE    0x01    /* Regular variable */
#define SYM_ARRAY       0x02    /* Array variable */
#define SYM_PARAMETER   0x04    /* Subroutine parameter */
#define SYM_DEFINED     0x08    /* Has been assigned a value */
#define SYM_COMMON      0x10    /* Variable is in COMMON block */

/*============================================================================
 * Symbol table entry
 *============================================================================*/
typedef struct {
    char     name[MAX_IDENT_LEN + 1];   /* Symbol name (uppercase) */
    uint8_t  type;                       /* TYPE_INTEGER, TYPE_REAL, etc. */
    uint8_t  flags;                      /* SYM_VARIABLE, SYM_ARRAY, etc. */
    uint8_t  num_dims;                   /* Number of dimensions (0=scalar, 1-3=array) */
    uint16_t dims[MAX_DIMS];             /* Size of each dimension */
    uint16_t addr;                       /* Address in variable storage */
} symbol_t;

/*============================================================================
 * Label table entry (for GOTO/DO targets)
 *============================================================================*/
typedef struct {
    uint16_t label;     /* Label number (1-99999) */
    uint16_t line;      /* Line number in program */
    uint16_t addr;      /* Address/offset in program text */
} label_t;

/*============================================================================
 * COMMON block entry
 *============================================================================*/
typedef struct {
    char     name[MAX_IDENT_LEN + 1];   /* Block name (empty = blank common) */
    uint16_t base_addr;                  /* Base address in variable storage */
    uint16_t next_offset;                /* Next available offset in block */
} common_block_t;

/*============================================================================
 * Symbol table state
 *============================================================================*/
typedef struct {
    symbol_t symbols[MAX_SYMBOLS];
    uint8_t  num_symbols;
    uint16_t next_addr;     /* Next available address in var storage */

    label_t  labels[MAX_LABELS];
    uint8_t  num_labels;

    common_block_t common_blocks[MAX_COMMON_BLOCKS];
    uint8_t num_common_blocks;
} symtab_t;

/*============================================================================
 * Global symbol table instance
 *============================================================================*/
extern symtab_t symtab;

/*============================================================================
 * Symbol table functions
 *============================================================================*/

/* Initialize symbol table (clears everything including labels) */
void sym_init_all(void);

/* Initialize symbol table (clears symbols but not labels) */
void sym_init(void);

/* Look up symbol by name, returns NULL if not found */
symbol_t *sym_lookup(const char *name);

/* Add a new symbol, returns pointer or NULL if table full */
symbol_t *sym_add(const char *name, uint8_t type, uint8_t flags, uint16_t size);

/* Add a multi-dimensional array symbol */
symbol_t *sym_add_array(const char *name, uint8_t type, uint8_t flags,
                        uint8_t num_dims, const uint16_t *dims);

/* Get total size of a symbol (product of all dimensions for arrays) */
uint16_t sym_total_size(const symbol_t *sym);

/* Get/set integer value */
int16_t sym_get_int(symbol_t *sym, uint16_t index);
void sym_set_int(symbol_t *sym, uint16_t index, int16_t value);

/* Get/set real (BCD) value */
void sym_get_real(symbol_t *sym, uint16_t index, bcd_t *value);
void sym_set_real(symbol_t *sym, uint16_t index, const bcd_t *value);

/*============================================================================
 * Label table functions
 *============================================================================*/

/* Clear all labels */
void label_init(void);

/* Add a label, returns FALSE if duplicate or table full */
bool_t label_add(uint16_t label, uint16_t line, uint16_t addr);

/* Look up label, returns NULL if not found */
label_t *label_lookup(uint16_t label);

/*============================================================================
 * COMMON block functions
 *============================================================================*/

/* Clear all COMMON blocks */
void common_init(void);

/* Find or create a COMMON block, returns NULL if table full */
common_block_t *common_find_or_create(const char *name);

/* Add a variable to a COMMON block, returns the symbol or NULL on error */
symbol_t *common_add_var(common_block_t *cb, const char *name, uint8_t type, uint16_t size);

#endif /* SYMTAB_H */
