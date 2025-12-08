/*
 * parser.h - Parser interface for kz80_fortran
 *
 * Recursive descent parser for Fortran 77 subset.
 */

#ifndef PARSER_H
#define PARSER_H

#include "types.h"
#include "lexer.h"

/*============================================================================
 * Parser state
 *============================================================================*/
typedef struct {
    lexer_t  lex;           /* Lexer state */
    uint8_t  error;         /* Current error code (ERR_NONE if ok) */
    uint16_t error_line;    /* Line where error occurred */
    char     error_msg[32]; /* Error message */
} parser_t;

/*============================================================================
 * Expression result
 *============================================================================*/
typedef struct {
    uint8_t type;       /* TYPE_INTEGER or TYPE_REAL */
    union {
        int16_t i;      /* Integer value */
        bcd_t   r;      /* Real value */
    } val;
} expr_result_t;

/*============================================================================
 * Parser initialization
 *============================================================================*/

/* Initialize parser with input line */
void parser_init(parser_t *p, const char *input, uint8_t line_num);

/* Check if parser has error */
bool_t parser_has_error(parser_t *p);

/* Get error message */
const char *parser_error_msg(parser_t *p);

/*============================================================================
 * Expression parsing (returns value directly for interpreter)
 *============================================================================*/

/* Parse and evaluate expression, result in 'result' */
bool_t parse_expression(parser_t *p, expr_result_t *result);

/* Parse and evaluate a single factor */
bool_t parse_factor(parser_t *p, expr_result_t *result);

/*============================================================================
 * Statement parsing
 *============================================================================*/

/* Parse and execute a single statement */
bool_t parse_statement(parser_t *p);

/* Parse variable declaration (INTEGER, REAL, DIMENSION) */
bool_t parse_declaration(parser_t *p);

/* Parse assignment statement */
bool_t parse_assignment(parser_t *p);

/* Parse IF statement */
bool_t parse_if(parser_t *p);

/* Parse DO loop */
bool_t parse_do(parser_t *p);

/* Parse WRITE statement */
bool_t parse_write(parser_t *p);

/* Parse READ statement */
bool_t parse_read(parser_t *p);

/* Parse SUBROUTINE definition */
bool_t parse_subroutine(parser_t *p);

/* Parse FUNCTION definition */
bool_t parse_function(parser_t *p);

/* Parse CALL statement */
bool_t parse_call(parser_t *p);

/* Parse RETURN statement */
bool_t parse_return(parser_t *p);

/*============================================================================
 * Program parsing
 *============================================================================*/

/* Parse PROGRAM statement */
bool_t parse_program(parser_t *p);

/* Parse complete program */
bool_t parse_program_body(parser_t *p);

/*============================================================================
 * Helper functions
 *============================================================================*/

/* Convert integer to real */
void int_to_real(expr_result_t *result);

/* Convert real to integer */
void real_to_int(expr_result_t *result);

/* Promote both operands to real if either is real */
void promote_operands(expr_result_t *a, expr_result_t *b);

#endif /* PARSER_H */
