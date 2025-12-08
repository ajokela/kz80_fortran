/*
 * lexer.h - Tokenizer interface for kz80_fortran
 */

#ifndef LEXER_H
#define LEXER_H

#include <stdint.h>
#include "types.h"

/*============================================================================
 * Token types
 *============================================================================*/
typedef enum {
    /* End of input / errors */
    TK_EOF = 0,
    TK_ERROR,
    TK_NEWLINE,

    /* Literals */
    TK_INT_LIT,         /* Integer literal: 123 */
    TK_REAL_LIT,        /* Real literal: 3.14, 1E-5 */
    TK_STRING_LIT,      /* String literal: 'Hello' */

    /* Identifier */
    TK_IDENT,           /* Identifier: MYVAR */

    /* Arithmetic operators */
    TK_PLUS,            /* + */
    TK_MINUS,           /* - */
    TK_STAR,            /* * */
    TK_SLASH,           /* / */
    TK_POWER,           /* ** */

    /* Relational operators */
    TK_EQ,              /* .EQ. or = (in conditions) */
    TK_NE,              /* .NE. */
    TK_LT,              /* .LT. */
    TK_GT,              /* .GT. */
    TK_LE,              /* .LE. */
    TK_GE,              /* .GE. */

    /* Logical operators */
    TK_AND,             /* .AND. */
    TK_OR,              /* .OR. */
    TK_NOT,             /* .NOT. */
    TK_TRUE,            /* .TRUE. */
    TK_FALSE,           /* .FALSE. */

    /* Punctuation */
    TK_LPAREN,          /* ( */
    TK_RPAREN,          /* ) */
    TK_COMMA,           /* , */
    TK_COLON,           /* : */
    TK_ASSIGN,          /* = (assignment context) */

    /* Keywords */
    TK_PROGRAM,
    TK_END,
    TK_INTEGER,
    TK_REAL,
    TK_LOGICAL,
    TK_DIMENSION,
    TK_DATA,
    TK_COMMON,
    TK_FORMAT,
    TK_IF,
    TK_THEN,
    TK_ELSE,
    TK_ENDIF,
    TK_DO,
    TK_CONTINUE,
    TK_GOTO,
    TK_WRITE,
    TK_READ,
    TK_STOP,
    TK_SUBROUTINE,
    TK_FUNCTION,
    TK_CALL,
    TK_RETURN,

    /* Statement label (numeric) */
    TK_LABEL,

    /* Total count */
    TK_COUNT
} token_type_t;

/*============================================================================
 * Token structure
 *============================================================================*/
typedef struct {
    token_type_t type;
    union {
        int16_t     int_val;            /* For TK_INT_LIT, TK_LABEL */
        bcd_t       real_val;           /* For TK_REAL_LIT */
        char        str_val[MAX_STRING_LEN + 1];  /* For TK_STRING_LIT, TK_IDENT */
    } value;
} token_t;

/*============================================================================
 * Keyword table entry
 *============================================================================*/
typedef struct {
    const char    *name;
    token_type_t   type;
} keyword_t;

/*============================================================================
 * Lexer state
 *============================================================================*/
typedef struct {
    const char *input;      /* Current input line */
    uint8_t     pos;        /* Current position in line */
    uint8_t     line_num;   /* Current line number (for errors) */
    token_t     current;    /* Current token */
    token_t     peek;       /* Lookahead token */
    bool_t      has_peek;   /* True if peek token is valid */
} lexer_t;

/*============================================================================
 * Lexer functions
 *============================================================================*/

/* Initialize lexer with input line */
void lexer_init(lexer_t *lex, const char *input, uint8_t line_num);

/* Get next token */
token_t *lexer_next(lexer_t *lex);

/* Peek at next token without consuming */
token_t *lexer_peek(lexer_t *lex);

/* Check if current token matches expected type */
bool_t lexer_match(lexer_t *lex, token_type_t expected);

/* Expect a specific token, return error if not found */
bool_t lexer_expect(lexer_t *lex, token_type_t expected);

/* Get current token */
token_t *lexer_current(lexer_t *lex);

/* Get token type name (for error messages) */
const char *token_name(token_type_t type);

/* Check if at end of line */
bool_t lexer_at_eol(lexer_t *lex);

#endif /* LEXER_H */
