/*
 * types.h - Common types and constants for kz80_fortran
 */

#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>

/*============================================================================
 * Boolean type
 *============================================================================*/
typedef uint8_t bool_t;
#define TRUE  1
#define FALSE 0

/*============================================================================
 * BCD Floating Point Number
 *
 * 6 bytes total:
 *   - 1 byte: sign (0 = positive, 0x80 = negative)
 *   - 1 byte: exponent (biased by 0x40, so 0x40 = 10^0)
 *   - 4 bytes: 8 BCD digits (mantissa, normalized so first digit != 0)
 *
 * Example: 123.45 = 0x00, 0x43, 0x12, 0x34, 0x50, 0x00
 *   sign=0, exp=0x43 (10^3), mantissa=0.12345000
 *   value = 0.12345 * 10^3 = 123.45
 *============================================================================*/
typedef struct {
    uint8_t sign;       /* 0x00 = positive, 0x80 = negative */
    uint8_t exp;        /* Exponent, biased by 0x40 */
    uint8_t digits[4];  /* 8 BCD digits packed into 4 bytes */
} bcd_t;

/* BCD constants */
#define BCD_EXP_BIAS    0x40
#define BCD_SIGN_POS    0x00
#define BCD_SIGN_NEG    0x80

/*============================================================================
 * Value union - can hold INTEGER or REAL
 *============================================================================*/
typedef struct {
    uint8_t type;       /* TYPE_INTEGER or TYPE_REAL */
    union {
        int16_t i;      /* Integer value */
        bcd_t   r;      /* Real (BCD) value */
    } v;
} value_t;

/* Value types */
#define TYPE_INTEGER    0
#define TYPE_REAL       1
#define TYPE_STRING     2
#define TYPE_LOGICAL    3
#define TYPE_LABEL      4

/*============================================================================
 * Error codes
 *============================================================================*/
#define ERR_NONE            0
#define ERR_SYNTAX          1   /* Syntax error */
#define ERR_UNDEFINED       2   /* Undefined variable */
#define ERR_TYPE_MISMATCH   3   /* Type mismatch */
#define ERR_OVERFLOW        4   /* Numeric overflow */
#define ERR_DIV_ZERO        5   /* Division by zero */
#define ERR_OUT_OF_MEMORY   6   /* Out of memory */
#define ERR_BAD_SUBSCRIPT   7   /* Array subscript out of bounds */
#define ERR_UNDEFINED_LABEL 8   /* Undefined label */
#define ERR_DUP_LABEL       9   /* Duplicate label */
#define ERR_NO_PROGRAM      10  /* No program in memory */
#define ERR_STACK_OVERFLOW  11  /* Expression stack overflow */
#define ERR_EXPECTED        12  /* Expected token not found */
#define ERR_RANGE           13  /* Value out of range (e.g., sqrt of negative) */

/*============================================================================
 * Memory layout constants
 *
 * Layout:
 *   0x0100-0x54FF  Code + GSINIT (~21KB)
 *   0x5500-0x66FF  C globals/DATA section (~4.5KB) - managed by linker
 *   0x6700-0x71FF  Program text storage (~2.8KB)
 *   0x7200-0x7DFF  Variable storage (~3KB)
 *   0x8000         Stack (grows down)
 *============================================================================*/
#define MEM_PROGRAM     0x6700  /* Program text storage */
#define MEM_PROG_SIZE   0x0B00  /* 2.8KB for program */
#define MEM_VARS        0x7200  /* Variable storage */
#define MEM_VARS_SIZE   0x0C00  /* 3KB for variables */

/*============================================================================
 * Limits
 *============================================================================*/
#define MAX_IDENT_LEN   10      /* Maximum identifier length (DIMENSION=9) */
#define MAX_LINE_LEN    80      /* Maximum source line length */
#define MAX_STRING_LEN  80      /* Maximum string literal length */
#define MAX_SYMBOLS     128     /* Maximum number of symbols */
#define MAX_LABELS      64      /* Maximum number of labels */
#define MAX_EXPR_DEPTH  16      /* Maximum expression nesting */
#define MAX_PROG_LINES  32      /* Maximum program lines */
#define MAX_DO_NEST     8       /* Maximum DO loop nesting */
#define MAX_DIMS        3       /* Maximum array dimensions (up to 3D) */
#define MAX_SUBPROGRAMS 8       /* Maximum subroutines/functions */
#define MAX_PARAMS      8       /* Maximum parameters per subprogram */
#define MAX_CALL_DEPTH  8       /* Maximum call nesting depth */
#define MAX_COMMON_BLOCKS 4     /* Maximum COMMON blocks */
#define MAX_COMMON_VARS   8     /* Maximum variables per COMMON block */

#endif /* TYPES_H */
