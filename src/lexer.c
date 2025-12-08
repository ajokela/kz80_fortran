/*
 * lexer.c - Tokenizer for kz80_fortran
 *
 * Free-format Fortran 77 lexer with case-insensitive keywords.
 */

#include "lexer.h"
#include "bcd.h"
#include <string.h>

/*============================================================================
 * Keyword table (must be sorted for binary search or use linear search)
 *============================================================================*/
static const keyword_t keywords[] = {
    { "AND",        TK_AND },
    { "CALL",       TK_CALL },
    { "COMMON",     TK_COMMON },
    { "CONTINUE",   TK_CONTINUE },
    { "DATA",       TK_DATA },
    { "DIMENSION",  TK_DIMENSION },
    { "DO",         TK_DO },
    { "ELSE",       TK_ELSE },
    { "END",        TK_END },
    { "ENDIF",      TK_ENDIF },
    { "EQ",         TK_EQ },
    { "FALSE",      TK_FALSE },
    { "FORMAT",     TK_FORMAT },
    { "FUNCTION",   TK_FUNCTION },
    { "GE",         TK_GE },
    { "GOTO",       TK_GOTO },
    { "GT",         TK_GT },
    { "IF",         TK_IF },
    { "INTEGER",    TK_INTEGER },
    { "LE",         TK_LE },
    { "LOGICAL",    TK_LOGICAL },
    { "LT",         TK_LT },
    { "NE",         TK_NE },
    { "NOT",        TK_NOT },
    { "OR",         TK_OR },
    { "PROGRAM",    TK_PROGRAM },
    { "READ",       TK_READ },
    { "REAL",       TK_REAL },
    { "RETURN",     TK_RETURN },
    { "STOP",       TK_STOP },
    { "SUBROUTINE", TK_SUBROUTINE },
    { "THEN",       TK_THEN },
    { "TRUE",       TK_TRUE },
    { "WRITE",      TK_WRITE },
    { NULL,         TK_EOF }  /* Sentinel */
};

#define NUM_KEYWORDS (sizeof(keywords) / sizeof(keywords[0]) - 1)

/*============================================================================
 * Character classification helpers
 *============================================================================*/
static bool_t is_alpha(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_';
}

static bool_t is_digit(char c) {
    return c >= '0' && c <= '9';
}

static bool_t is_alnum(char c) {
    return is_alpha(c) || is_digit(c);
}

static bool_t is_space(char c) {
    return c == ' ' || c == '\t';
}

static char to_upper(char c) {
    if (c >= 'a' && c <= 'z') {
        return c - 'a' + 'A';
    }
    return c;
}

/*============================================================================
 * Skip whitespace (not newlines)
 *============================================================================*/
static void skip_whitespace(lexer_t *lex) {
    while (is_space(lex->input[lex->pos])) {
        lex->pos++;
    }
}

/*============================================================================
 * Skip to end of line (for comments)
 *============================================================================*/
static void skip_comment(lexer_t *lex) {
    while (lex->input[lex->pos] != '\0' && lex->input[lex->pos] != '\n') {
        lex->pos++;
    }
}

/*============================================================================
 * Look up keyword
 *============================================================================*/
static token_type_t lookup_keyword(const char *name) {
    for (uint8_t i = 0; i < NUM_KEYWORDS; i++) {
        if (strcmp(name, keywords[i].name) == 0) {
            return keywords[i].type;
        }
    }
    return TK_IDENT;  /* Not a keyword */
}

/*============================================================================
 * Parse number (integer or real)
 *============================================================================*/
static void parse_number(lexer_t *lex, token_t *tok) {
    uint8_t start_pos = lex->pos;
    int16_t int_val = 0;
    bool_t has_decimal = FALSE;

    /* Parse integer part */
    while (is_digit(lex->input[lex->pos])) {
        int_val = int_val * 10 + (lex->input[lex->pos] - '0');
        lex->pos++;
    }

    /* Check for decimal point */
    if (lex->input[lex->pos] == '.') {
        char next = lex->input[lex->pos + 1];
        /* Only treat as decimal if followed by digit or end/operator */
        /* Not if followed by letter (could be .AND. etc after integer) */
        if (is_digit(next) || next == '\0' || next == '\n' ||
            next == ' ' || next == '+' || next == '-' ||
            next == '*' || next == '/' || next == ')' || next == ',') {
            has_decimal = TRUE;
        }
    }

    /* Check for exponent (E or D) */
    if (!has_decimal) {
        char c = to_upper(lex->input[lex->pos]);
        if (c == 'E' || c == 'D') {
            has_decimal = TRUE;
        }
    }

    if (has_decimal) {
        /* It's a real literal - use BCD parsing */
        /* Build a temp string from start_pos to current pos, then continue */
        char num_str[32];
        uint8_t len = 0;

        /* Reset and re-parse as real */
        lex->pos = start_pos;

        /* Copy digits, decimal, exponent */
        while (len < 31) {
            char c = lex->input[lex->pos];
            if (is_digit(c) || c == '.') {
                num_str[len++] = c;
                lex->pos++;
            } else if (to_upper(c) == 'E' || to_upper(c) == 'D') {
                num_str[len++] = 'E';
                lex->pos++;
                /* Handle optional sign */
                if (lex->input[lex->pos] == '+' || lex->input[lex->pos] == '-') {
                    num_str[len++] = lex->input[lex->pos];
                    lex->pos++;
                }
            } else {
                break;
            }
        }
        num_str[len] = '\0';

        /* Parse to BCD */
        bcd_parse(&tok->value.real_val, num_str);
        tok->type = TK_REAL_LIT;
    } else {
        tok->type = TK_INT_LIT;
        tok->value.int_val = int_val;
    }
}

/*============================================================================
 * Parse identifier or keyword
 *============================================================================*/
static void parse_identifier(lexer_t *lex, token_t *tok) {
    uint8_t len = 0;
    char name[MAX_IDENT_LEN + 1];

    /* Collect identifier characters */
    while (is_alnum(lex->input[lex->pos]) && len < MAX_IDENT_LEN) {
        name[len++] = to_upper(lex->input[lex->pos]);
        lex->pos++;
    }
    name[len] = '\0';

    /* Skip remaining characters if identifier is too long */
    while (is_alnum(lex->input[lex->pos])) {
        lex->pos++;
    }

    /* Look up as keyword */
    token_type_t kw = lookup_keyword(name);
    if (kw != TK_IDENT) {
        tok->type = kw;
    } else {
        tok->type = TK_IDENT;
        strcpy(tok->value.str_val, name);
    }
}

/*============================================================================
 * Parse dot operator (.EQ., .AND., etc.)
 *============================================================================*/
static void parse_dot_operator(lexer_t *lex, token_t *tok) {
    char name[MAX_IDENT_LEN + 1];
    uint8_t len = 0;

    lex->pos++;  /* Skip first dot */

    /* Collect letters */
    while (is_alpha(lex->input[lex->pos]) && len < MAX_IDENT_LEN) {
        name[len++] = to_upper(lex->input[lex->pos]);
        lex->pos++;
    }
    name[len] = '\0';

    /* Expect closing dot */
    if (lex->input[lex->pos] != '.') {
        tok->type = TK_ERROR;
        return;
    }
    lex->pos++;  /* Skip closing dot */

    /* Look up operator */
    token_type_t kw = lookup_keyword(name);
    if (kw != TK_IDENT) {
        tok->type = kw;
    } else {
        tok->type = TK_ERROR;
    }
}

/*============================================================================
 * Parse string literal
 *============================================================================*/
static void parse_string(lexer_t *lex, token_t *tok) {
    uint8_t len = 0;

    lex->pos++;  /* Skip opening quote */

    while (lex->input[lex->pos] != '\0' && len < MAX_STRING_LEN) {
        if (lex->input[lex->pos] == '\'') {
            /* Check for escaped quote ('') */
            if (lex->input[lex->pos + 1] == '\'') {
                tok->value.str_val[len++] = '\'';
                lex->pos += 2;
            } else {
                /* End of string */
                lex->pos++;
                break;
            }
        } else {
            tok->value.str_val[len++] = lex->input[lex->pos];
            lex->pos++;
        }
    }

    tok->value.str_val[len] = '\0';
    tok->type = TK_STRING_LIT;
}

/*============================================================================
 * Get next token (main lexer function)
 *============================================================================*/
static void scan_token(lexer_t *lex, token_t *tok) {
    skip_whitespace(lex);

    char c = lex->input[lex->pos];

    /* End of line */
    if (c == '\0' || c == '\n') {
        tok->type = TK_NEWLINE;
        return;
    }

    /* Comment (Fortran 77 uses ! for inline comments in free format) */
    if (c == '!') {
        skip_comment(lex);
        tok->type = TK_NEWLINE;
        return;
    }

    /* Number (integer or real) */
    if (is_digit(c)) {
        parse_number(lex, tok);
        return;
    }

    /* Identifier or keyword */
    if (is_alpha(c)) {
        parse_identifier(lex, tok);
        return;
    }

    /* String literal */
    if (c == '\'') {
        parse_string(lex, tok);
        return;
    }

    /* Dot operator or decimal point */
    if (c == '.') {
        if (is_alpha(lex->input[lex->pos + 1])) {
            parse_dot_operator(lex, tok);
        } else {
            /* Could be start of .5 style real, or just a dot */
            tok->type = TK_ERROR;
            lex->pos++;
        }
        return;
    }

    /* Two-character operators */
    if (c == '*' && lex->input[lex->pos + 1] == '*') {
        tok->type = TK_POWER;
        lex->pos += 2;
        return;
    }

    /* Single-character tokens */
    lex->pos++;
    switch (c) {
        case '+': tok->type = TK_PLUS;   break;
        case '-': tok->type = TK_MINUS;  break;
        case '*': tok->type = TK_STAR;   break;
        case '/': tok->type = TK_SLASH;  break;
        case '(': tok->type = TK_LPAREN; break;
        case ')': tok->type = TK_RPAREN; break;
        case ',': tok->type = TK_COMMA;  break;
        case ':': tok->type = TK_COLON;  break;
        case '=': tok->type = TK_ASSIGN; break;
        default:  tok->type = TK_ERROR;  break;
    }
}

/*============================================================================
 * Public functions
 *============================================================================*/

void lexer_init(lexer_t *lex, const char *input, uint8_t line_num) {
    lex->input = input;
    lex->pos = 0;
    lex->line_num = line_num;
    lex->has_peek = FALSE;

    /* Get first token */
    scan_token(lex, &lex->current);
}

token_t *lexer_next(lexer_t *lex) {
    if (lex->has_peek) {
        lex->current = lex->peek;
        lex->has_peek = FALSE;
    } else {
        scan_token(lex, &lex->current);
    }
    return &lex->current;
}

token_t *lexer_peek(lexer_t *lex) {
    if (!lex->has_peek) {
        scan_token(lex, &lex->peek);
        lex->has_peek = TRUE;
    }
    return &lex->peek;
}

bool_t lexer_match(lexer_t *lex, token_type_t expected) {
    if (lex->current.type == expected) {
        lexer_next(lex);
        return TRUE;
    }
    return FALSE;
}

bool_t lexer_expect(lexer_t *lex, token_type_t expected) {
    if (lex->current.type != expected) {
        return FALSE;
    }
    lexer_next(lex);
    return TRUE;
}

token_t *lexer_current(lexer_t *lex) {
    return &lex->current;
}

bool_t lexer_at_eol(lexer_t *lex) {
    return lex->current.type == TK_NEWLINE || lex->current.type == TK_EOF;
}

const char *token_name(token_type_t type) {
    static const char *names[] = {
        "EOF", "ERROR", "NEWLINE",
        "INT", "REAL", "STRING",
        "IDENT",
        "+", "-", "*", "/", "**",
        ".EQ.", ".NE.", ".LT.", ".GT.", ".LE.", ".GE.",
        ".AND.", ".OR.", ".NOT.", ".TRUE.", ".FALSE.",
        "(", ")", ",", ":", "=",
        "PROGRAM", "END", "INTEGER", "REAL", "LOGICAL",
        "DIMENSION", "IF", "THEN", "ELSE", "ENDIF",
        "DO", "CONTINUE", "GOTO", "WRITE", "READ", "STOP",
        "SUBROUTINE", "FUNCTION", "CALL", "RETURN",
        "LABEL"
    };

    if (type < TK_COUNT) {
        return names[type];
    }
    return "?";
}
