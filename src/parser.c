/*
 * parser.c - Recursive descent parser for kz80_fortran
 *
 * Expression grammar (precedence low to high):
 *   expr     = or_expr
 *   or_expr  = and_expr { .OR. and_expr }
 *   and_expr = not_expr { .AND. not_expr }
 *   not_expr = [ .NOT. ] rel_expr
 *   rel_expr = arith_expr { relop arith_expr }
 *   arith_expr = term { (+|-) term }
 *   term     = power { (*|/) power }
 *   power    = unary { ** unary }
 *   unary    = [+|-] factor
 *   factor   = literal | variable | ( expr ) | function_call
 */

#include "config.h"
#include "parser.h"
#include "symtab.h"
#include "bcd.h"
#include "acia.h"
#include "program.h"
#include <string.h>

/*============================================================================
 * Parser initialization
 *============================================================================*/

void parser_init(parser_t *p, const char *input, uint8_t line_num) {
    lexer_init(&p->lex, input, line_num);
    p->error = ERR_NONE;
    p->error_line = line_num;
    p->error_msg[0] = '\0';
}

bool_t parser_has_error(parser_t *p) {
    return p->error != ERR_NONE;
}

const char *parser_error_msg(parser_t *p) {
    return p->error_msg;
}

static void set_error(parser_t *p, uint8_t code, const char *msg) {
    if (p->error == ERR_NONE) {
        p->error = code;
        p->error_line = p->lex.line_num;
        strncpy(p->error_msg, msg, 31);
        p->error_msg[31] = '\0';
    }
}

/*============================================================================
 * Type conversion helpers
 *============================================================================*/

void int_to_real(expr_result_t *result) {
    if (result->type == TYPE_INTEGER) {
        int16_t i = result->val.i;
        bcd_from_int(&result->val.r, i);
        result->type = TYPE_REAL;
    }
}

void real_to_int(expr_result_t *result) {
    if (result->type == TYPE_REAL) {
        result->val.i = bcd_to_int(&result->val.r);
        result->type = TYPE_INTEGER;
    }
}

void promote_operands(expr_result_t *a, expr_result_t *b) {
    if (a->type == TYPE_REAL || b->type == TYPE_REAL) {
        int_to_real(a);
        int_to_real(b);
    }
}

/*============================================================================
 * Forward declarations
 *============================================================================*/
static bool_t parse_or_expr(parser_t *p, expr_result_t *result);
static bool_t parse_and_expr(parser_t *p, expr_result_t *result);
static bool_t parse_not_expr(parser_t *p, expr_result_t *result);
static bool_t parse_rel_expr(parser_t *p, expr_result_t *result);
static bool_t parse_arith_expr(parser_t *p, expr_result_t *result);
static bool_t parse_term(parser_t *p, expr_result_t *result);
static bool_t parse_power(parser_t *p, expr_result_t *result);
static bool_t parse_unary(parser_t *p, expr_result_t *result);

/*============================================================================
 * Factor: literal, variable, parenthesized expression
 *============================================================================*/
bool_t parse_factor(parser_t *p, expr_result_t *result) {
    token_t *tok = lexer_current(&p->lex);

    if (tok->type == TK_INT_LIT) {
        result->type = TYPE_INTEGER;
        result->val.i = tok->value.int_val;
        lexer_next(&p->lex);
        return TRUE;
    }

    if (tok->type == TK_REAL_LIT) {
        result->type = TYPE_REAL;
        bcd_copy(&result->val.r, &tok->value.real_val);
        lexer_next(&p->lex);
        return TRUE;
    }

    if (tok->type == TK_TRUE) {
        result->type = TYPE_INTEGER;
        result->val.i = 1;
        lexer_next(&p->lex);
        return TRUE;
    }

    if (tok->type == TK_FALSE) {
        result->type = TYPE_INTEGER;
        result->val.i = 0;
        lexer_next(&p->lex);
        return TRUE;
    }

    /* Handle REAL(x) intrinsic function (REAL is a keyword) */
    if (tok->type == TK_REAL) {
        lexer_next(&p->lex);  /* Skip REAL */
        if (lexer_current(&p->lex)->type == TK_LPAREN) {
            lexer_next(&p->lex);  /* Skip ( */
            expr_result_t arg;
            if (!parse_expression(p, &arg)) return FALSE;
            if (!lexer_expect(&p->lex, TK_RPAREN)) {
                set_error(p, ERR_EXPECTED, "Expected )");
                return FALSE;
            }
            int_to_real(&arg);
            result->type = TYPE_REAL;
            bcd_copy(&result->val.r, &arg.val.r);
            return TRUE;
        }
        /* Not followed by (, treat as error in expression context */
        set_error(p, ERR_SYNTAX, "Unexpected REAL");
        return FALSE;
    }

    if (tok->type == TK_IDENT) {
        char name[MAX_IDENT_LEN + 1];
        strcpy(name, tok->value.str_val);
        lexer_next(&p->lex);

        /* Check for function call or array subscript */
        if (lexer_current(&p->lex)->type == TK_LPAREN) {
            /* Could be intrinsic function or array */

#ifdef FEATURE_INTRINSICS
            /* ABS(x) - absolute value */
            if (strcmp(name, "ABS") == 0) {
                lexer_next(&p->lex);  /* Skip ( */
                expr_result_t arg;
                if (!parse_expression(p, &arg)) return FALSE;
                if (!lexer_expect(&p->lex, TK_RPAREN)) {
                    set_error(p, ERR_EXPECTED, "Expected )");
                    return FALSE;
                }
                if (arg.type == TYPE_INTEGER) {
                    result->type = TYPE_INTEGER;
                    result->val.i = (arg.val.i < 0) ? -arg.val.i : arg.val.i;
                } else {
                    result->type = TYPE_REAL;
                    bcd_abs(&result->val.r, &arg.val.r);
                }
                return TRUE;
            }

            /* MOD(a, b) - modulo */
            if (strcmp(name, "MOD") == 0) {
                lexer_next(&p->lex);  /* Skip ( */
                expr_result_t a, b;
                if (!parse_expression(p, &a)) return FALSE;
                if (!lexer_expect(&p->lex, TK_COMMA)) {
                    set_error(p, ERR_EXPECTED, "Expected ,");
                    return FALSE;
                }
                if (!parse_expression(p, &b)) return FALSE;
                if (!lexer_expect(&p->lex, TK_RPAREN)) {
                    set_error(p, ERR_EXPECTED, "Expected )");
                    return FALSE;
                }
                real_to_int(&a);
                real_to_int(&b);
                result->type = TYPE_INTEGER;
                if (b.val.i != 0) {
                    result->val.i = a.val.i % b.val.i;
                } else {
                    result->val.i = 0;
                }
                return TRUE;
            }

            /* INT(x) - convert to integer */
            if (strcmp(name, "INT") == 0) {
                lexer_next(&p->lex);  /* Skip ( */
                expr_result_t arg;
                if (!parse_expression(p, &arg)) return FALSE;
                if (!lexer_expect(&p->lex, TK_RPAREN)) {
                    set_error(p, ERR_EXPECTED, "Expected )");
                    return FALSE;
                }
                real_to_int(&arg);
                result->type = TYPE_INTEGER;
                result->val.i = arg.val.i;
                return TRUE;
            }
#endif /* FEATURE_INTRINSICS */

#ifdef FEATURE_SQRT
            /* SQRT(x) - square root */
            if (strcmp(name, "SQRT") == 0) {
                lexer_next(&p->lex);  /* Skip ( */
                expr_result_t arg;
                if (!parse_expression(p, &arg)) return FALSE;
                if (!lexer_expect(&p->lex, TK_RPAREN)) {
                    set_error(p, ERR_EXPECTED, "Expected )");
                    return FALSE;
                }
                /* Convert to real if integer */
                if (arg.type == TYPE_INTEGER) {
                    int_to_real(&arg);
                }
                /* Compute square root */
                if (!bcd_sqrt(&result->val.r, &arg.val.r)) {
                    set_error(p, ERR_RANGE, "SQRT of negative");
                    return FALSE;
                }
                result->type = TYPE_REAL;
                return TRUE;
            }
#endif /* FEATURE_SQRT */

#ifdef FEATURE_SUBPROGRAMS
            /* Check for user-defined function */
            subprog_t *func = prog_find_subprog(name);
            if (func != NULL && func->type == SUBPROG_FUNCTION) {
                /* User-defined function call - implementation in full build */
                set_error(p, ERR_SYNTAX, "Function calls not implemented");
                return FALSE;
            }
#endif /* FEATURE_SUBPROGRAMS */

            /* Not an intrinsic or function - must be array subscript */
            lexer_next(&p->lex);  /* Skip ( */

            /* Look up array variable first to get dimensions */
            symbol_t *sym = sym_lookup(name);
            if (sym == NULL) {
                set_error(p, ERR_UNDEFINED, "Undefined variable");
                return FALSE;
            }

            /* Parse subscript list */
#ifdef FEATURE_MULTIDIM_ARRAYS
            uint16_t indices[MAX_DIMS];
            uint8_t num_indices = 0;

            do {
                if (num_indices >= MAX_DIMS) {
                    set_error(p, ERR_SYNTAX, "Too many subscripts");
                    return FALSE;
                }

                expr_result_t subscript;
                if (!parse_expression(p, &subscript)) {
                    return FALSE;
                }
                real_to_int(&subscript);
                indices[num_indices] = (uint16_t)(subscript.val.i - 1);  /* Fortran arrays are 1-based */
                num_indices++;

            } while (lexer_match(&p->lex, TK_COMMA));

            if (!lexer_expect(&p->lex, TK_RPAREN)) {
                set_error(p, ERR_EXPECTED, "Expected )");
                return FALSE;
            }

            /* Calculate linear index using row-major order:
             * For ARR(i,j,k) with dims (d1,d2,d3):
             * linear = i*d2*d3 + j*d3 + k
             */
            uint16_t linear_index = 0;
            uint16_t multiplier = 1;
            for (int8_t d = (int8_t)(sym->num_dims > 0 ? sym->num_dims : 1) - 1; d >= 0; d--) {
                if (d < num_indices) {
                    linear_index += indices[d] * multiplier;
                }
                multiplier *= sym->dims[d];
            }
#else
            /* Single dimension only */
            expr_result_t subscript;
            if (!parse_expression(p, &subscript)) {
                return FALSE;
            }
            real_to_int(&subscript);
            uint16_t linear_index = (uint16_t)(subscript.val.i - 1);

            if (!lexer_expect(&p->lex, TK_RPAREN)) {
                set_error(p, ERR_EXPECTED, "Expected )");
                return FALSE;
            }
#endif /* FEATURE_MULTIDIM_ARRAYS */

            /* Get value */
            if (sym->type == TYPE_INTEGER) {
                result->type = TYPE_INTEGER;
                result->val.i = sym_get_int(sym, linear_index);
            } else {
                result->type = TYPE_REAL;
                sym_get_real(sym, linear_index, &result->val.r);
            }
            return TRUE;
        }

        /* No parenthesis - simple variable */
        symbol_t *sym = sym_lookup(name);
        if (sym == NULL) {
            /* Auto-declare based on implicit typing (I-N = INTEGER, else REAL) */
            uint8_t type;
            if (name[0] >= 'I' && name[0] <= 'N') {
                type = TYPE_INTEGER;
            } else {
                type = TYPE_REAL;
            }
            sym = sym_add(name, type, SYM_VARIABLE, 1);
            if (sym == NULL) {
                set_error(p, ERR_OUT_OF_MEMORY, "Symbol table full");
                return FALSE;
            }
        }

        /* Get value */
        if (sym->type == TYPE_INTEGER) {
            result->type = TYPE_INTEGER;
            result->val.i = sym_get_int(sym, 0);
        } else {
            result->type = TYPE_REAL;
            sym_get_real(sym, 0, &result->val.r);
        }

        return TRUE;
    }

    if (tok->type == TK_LPAREN) {
        lexer_next(&p->lex);  /* Skip ( */

        if (!parse_expression(p, result)) {
            return FALSE;
        }

        if (!lexer_expect(&p->lex, TK_RPAREN)) {
            set_error(p, ERR_EXPECTED, "Expected )");
            return FALSE;
        }
        return TRUE;
    }

    set_error(p, ERR_SYNTAX, "Expected value");
    return FALSE;
}

/*============================================================================
 * Unary: optional +/- followed by factor
 *============================================================================*/
static bool_t parse_unary(parser_t *p, expr_result_t *result) {
    token_t *tok = lexer_current(&p->lex);
    bool_t negate = FALSE;

    if (tok->type == TK_PLUS) {
        lexer_next(&p->lex);
    } else if (tok->type == TK_MINUS) {
        negate = TRUE;
        lexer_next(&p->lex);
    }

    if (!parse_factor(p, result)) {
        return FALSE;
    }

    if (negate) {
        if (result->type == TYPE_INTEGER) {
            result->val.i = -result->val.i;
        } else {
            bcd_neg(&result->val.r);
        }
    }

    return TRUE;
}

/*============================================================================
 * Power: unary ** unary (right associative)
 *============================================================================*/
static bool_t parse_power(parser_t *p, expr_result_t *result) {
    if (!parse_unary(p, result)) {
        return FALSE;
    }

    while (lexer_current(&p->lex)->type == TK_POWER) {
        lexer_next(&p->lex);

        expr_result_t right;
        if (!parse_unary(p, &right)) {
            return FALSE;
        }

        /* For simplicity, only support integer exponents */
        real_to_int(&right);
        int16_t exp = right.val.i;

        if (result->type == TYPE_INTEGER) {
            int16_t base = result->val.i;
            int16_t res = 1;
            bool_t neg_exp = (exp < 0);
            if (neg_exp) exp = -exp;

            while (exp > 0) {
                res *= base;
                exp--;
            }

            if (neg_exp) {
                result->val.i = (res != 0) ? 1 / res : 0;
            } else {
                result->val.i = res;
            }
        } else {
            /* Real exponentiation - simplified */
            bcd_t base;
            bcd_copy(&base, &result->val.r);
            bcd_t res;
            bcd_from_int(&res, 1);

            bool_t neg_exp = (exp < 0);
            if (neg_exp) exp = -exp;

            while (exp > 0) {
                bcd_mul(&res, &res, &base);
                exp--;
            }

            if (neg_exp) {
                bcd_t one;
                bcd_from_int(&one, 1);
                bcd_div(&result->val.r, &one, &res);
            } else {
                bcd_copy(&result->val.r, &res);
            }
        }
    }

    return TRUE;
}

/*============================================================================
 * Term: power { (*|/) power }
 *============================================================================*/
static bool_t parse_term(parser_t *p, expr_result_t *result) {
    if (!parse_power(p, result)) {
        return FALSE;
    }

    while (1) {
        token_type_t op = lexer_current(&p->lex)->type;
        if (op != TK_STAR && op != TK_SLASH) {
            break;
        }
        lexer_next(&p->lex);

        expr_result_t right;
        if (!parse_power(p, &right)) {
            return FALSE;
        }

        promote_operands(result, &right);

        if (result->type == TYPE_INTEGER) {
            if (op == TK_STAR) {
                result->val.i = result->val.i * right.val.i;
            } else {
                if (right.val.i == 0) {
                    set_error(p, ERR_DIV_ZERO, "Division by zero");
                    return FALSE;
                }
                result->val.i = result->val.i / right.val.i;
            }
        } else {
            if (op == TK_STAR) {
                bcd_mul(&result->val.r, &result->val.r, &right.val.r);
            } else {
                if (!bcd_div(&result->val.r, &result->val.r, &right.val.r)) {
                    set_error(p, ERR_DIV_ZERO, "Division by zero");
                    return FALSE;
                }
            }
        }
    }

    return TRUE;
}

/*============================================================================
 * Arithmetic expression: term { (+|-) term }
 *============================================================================*/
static bool_t parse_arith_expr(parser_t *p, expr_result_t *result) {
    if (!parse_term(p, result)) {
        return FALSE;
    }

    while (1) {
        token_type_t op = lexer_current(&p->lex)->type;
        if (op != TK_PLUS && op != TK_MINUS) {
            break;
        }
        lexer_next(&p->lex);

        expr_result_t right;
        if (!parse_term(p, &right)) {
            return FALSE;
        }

        promote_operands(result, &right);

        if (result->type == TYPE_INTEGER) {
            if (op == TK_PLUS) {
                result->val.i = result->val.i + right.val.i;
            } else {
                result->val.i = result->val.i - right.val.i;
            }
        } else {
            if (op == TK_PLUS) {
                bcd_add(&result->val.r, &result->val.r, &right.val.r);
            } else {
                bcd_sub(&result->val.r, &result->val.r, &right.val.r);
            }
        }
    }

    return TRUE;
}

/*============================================================================
 * Relational expression: arith { relop arith }
 *============================================================================*/
static bool_t parse_rel_expr(parser_t *p, expr_result_t *result) {
    if (!parse_arith_expr(p, result)) {
        return FALSE;
    }

    token_type_t op = lexer_current(&p->lex)->type;
    if (op >= TK_EQ && op <= TK_GE) {
        lexer_next(&p->lex);

        expr_result_t right;
        if (!parse_arith_expr(p, &right)) {
            return FALSE;
        }

        promote_operands(result, &right);

        int8_t cmp;
        if (result->type == TYPE_INTEGER) {
            if (result->val.i < right.val.i) cmp = -1;
            else if (result->val.i > right.val.i) cmp = 1;
            else cmp = 0;
        } else {
            cmp = bcd_cmp(&result->val.r, &right.val.r);
        }

        bool_t res;
        switch (op) {
            case TK_EQ: res = (cmp == 0); break;
            case TK_NE: res = (cmp != 0); break;
            case TK_LT: res = (cmp < 0); break;
            case TK_GT: res = (cmp > 0); break;
            case TK_LE: res = (cmp <= 0); break;
            case TK_GE: res = (cmp >= 0); break;
            default: res = FALSE;
        }

        result->type = TYPE_INTEGER;
        result->val.i = res ? 1 : 0;
    }

    return TRUE;
}

/*============================================================================
 * NOT expression: [.NOT.] rel_expr
 *============================================================================*/
static bool_t parse_not_expr(parser_t *p, expr_result_t *result) {
    bool_t do_not = FALSE;

    if (lexer_current(&p->lex)->type == TK_NOT) {
        do_not = TRUE;
        lexer_next(&p->lex);
    }

    if (!parse_rel_expr(p, result)) {
        return FALSE;
    }

    if (do_not) {
        real_to_int(result);
        result->val.i = (result->val.i == 0) ? 1 : 0;
    }

    return TRUE;
}

/*============================================================================
 * AND expression: not_expr { .AND. not_expr }
 *============================================================================*/
static bool_t parse_and_expr(parser_t *p, expr_result_t *result) {
    if (!parse_not_expr(p, result)) {
        return FALSE;
    }

    while (lexer_current(&p->lex)->type == TK_AND) {
        lexer_next(&p->lex);

        expr_result_t right;
        if (!parse_not_expr(p, &right)) {
            return FALSE;
        }

        real_to_int(result);
        real_to_int(&right);

        result->val.i = (result->val.i != 0 && right.val.i != 0) ? 1 : 0;
    }

    return TRUE;
}

/*============================================================================
 * OR expression: and_expr { .OR. and_expr }
 *============================================================================*/
static bool_t parse_or_expr(parser_t *p, expr_result_t *result) {
    if (!parse_and_expr(p, result)) {
        return FALSE;
    }

    while (lexer_current(&p->lex)->type == TK_OR) {
        lexer_next(&p->lex);

        expr_result_t right;
        if (!parse_and_expr(p, &right)) {
            return FALSE;
        }

        real_to_int(result);
        real_to_int(&right);

        result->val.i = (result->val.i != 0 || right.val.i != 0) ? 1 : 0;
    }

    return TRUE;
}

/*============================================================================
 * Top-level expression
 *============================================================================*/
bool_t parse_expression(parser_t *p, expr_result_t *result) {
    return parse_or_expr(p, result);
}

/*============================================================================
 * Statement parsing
 *============================================================================*/

/* Parse assignment: variable = expression */
bool_t parse_assignment(parser_t *p) {
    token_t *tok = lexer_current(&p->lex);

    if (tok->type != TK_IDENT) {
        set_error(p, ERR_SYNTAX, "Expected variable");
        return FALSE;
    }

    char name[MAX_IDENT_LEN + 1];
    strcpy(name, tok->value.str_val);
    lexer_next(&p->lex);

    /* Check for array subscript(s) */
    uint16_t indices[MAX_DIMS];
    uint8_t num_indices = 0;
    bool_t is_array_access = FALSE;

    if (lexer_current(&p->lex)->type == TK_LPAREN) {
        lexer_next(&p->lex);
        is_array_access = TRUE;

        /* Parse comma-separated subscript list */
        do {
            if (num_indices >= MAX_DIMS) {
                set_error(p, ERR_SYNTAX, "Too many subscripts");
                return FALSE;
            }

            expr_result_t subscript;
            if (!parse_expression(p, &subscript)) {
                return FALSE;
            }
            real_to_int(&subscript);
            indices[num_indices] = (uint16_t)(subscript.val.i - 1);  /* 1-based to 0-based */
            num_indices++;

        } while (lexer_match(&p->lex, TK_COMMA));

        if (!lexer_expect(&p->lex, TK_RPAREN)) {
            set_error(p, ERR_EXPECTED, "Expected )");
            return FALSE;
        }
    }

    if (!lexer_expect(&p->lex, TK_ASSIGN)) {
        set_error(p, ERR_EXPECTED, "Expected =");
        return FALSE;
    }

    expr_result_t value;
    if (!parse_expression(p, &value)) {
        return FALSE;
    }

    /* Look up or create variable */
    symbol_t *sym = sym_lookup(name);
    if (sym == NULL) {
        uint8_t type;
        if (name[0] >= 'I' && name[0] <= 'N') {
            type = TYPE_INTEGER;
        } else {
            type = TYPE_REAL;
        }
        sym = sym_add(name, type, SYM_VARIABLE, 1);
        if (sym == NULL) {
            set_error(p, ERR_OUT_OF_MEMORY, "Symbol table full");
            return FALSE;
        }
    }

    /* Calculate linear index for multi-dimensional arrays */
    uint16_t linear_index = 0;
    if (is_array_access) {
        uint16_t multiplier = 1;
        for (int8_t d = (int8_t)(sym->num_dims > 0 ? sym->num_dims : 1) - 1; d >= 0; d--) {
            if (d < num_indices) {
                linear_index += indices[d] * multiplier;
            }
            multiplier *= sym->dims[d];
        }
    }

    /* Store value */
    if (sym->type == TYPE_INTEGER) {
        real_to_int(&value);
        sym_set_int(sym, linear_index, value.val.i);
    } else {
        int_to_real(&value);
        sym_set_real(sym, linear_index, &value.val.r);
    }

    return TRUE;
}

/* Parse declaration: INTEGER, REAL, or DIMENSION */
bool_t parse_declaration(parser_t *p) {
    token_t *tok = lexer_current(&p->lex);
    uint8_t type;
    bool_t is_dimension = FALSE;

    if (tok->type == TK_INTEGER) {
        type = TYPE_INTEGER;
    } else if (tok->type == TK_REAL) {
        type = TYPE_REAL;
    } else if (tok->type == TK_DIMENSION) {
        is_dimension = TRUE;
        type = TYPE_REAL;  /* Will be determined by implicit rules */
    } else {
        set_error(p, ERR_SYNTAX, "Expected type");
        return FALSE;
    }
    lexer_next(&p->lex);

    /* Parse variable list */
    do {
        if (lexer_current(&p->lex)->type != TK_IDENT) {
            set_error(p, ERR_EXPECTED, "Expected variable name");
            return FALSE;
        }

        char name[MAX_IDENT_LEN + 1];
        strcpy(name, lexer_current(&p->lex)->value.str_val);
        lexer_next(&p->lex);

        uint16_t dims[MAX_DIMS] = {1, 1, 1};
        uint8_t num_dims = 0;

        /* Check for array dimensions */
        if (lexer_current(&p->lex)->type == TK_LPAREN) {
            lexer_next(&p->lex);

            /* Parse comma-separated dimension list */
            do {
                if (num_dims >= MAX_DIMS) {
                    set_error(p, ERR_SYNTAX, "Too many dimensions");
                    return FALSE;
                }

                expr_result_t dim;
                if (!parse_expression(p, &dim)) {
                    return FALSE;
                }
                real_to_int(&dim);
                dims[num_dims] = (uint16_t)dim.val.i;
                num_dims++;

            } while (lexer_match(&p->lex, TK_COMMA));

            if (!lexer_expect(&p->lex, TK_RPAREN)) {
                set_error(p, ERR_EXPECTED, "Expected )");
                return FALSE;
            }
        }

        /* For DIMENSION, use implicit typing */
        uint8_t var_type = type;
        if (is_dimension) {
            if (name[0] >= 'I' && name[0] <= 'N') {
                var_type = TYPE_INTEGER;
            } else {
                var_type = TYPE_REAL;
            }
        }

        /* Add symbol - use sym_add_array for multi-dimensional */
        if (num_dims > 1) {
            if (sym_add_array(name, var_type, SYM_VARIABLE, num_dims, dims) == NULL) {
                /* May already exist */
                symbol_t *sym = sym_lookup(name);
                if (sym == NULL) {
                    set_error(p, ERR_OUT_OF_MEMORY, "Symbol table full");
                    return FALSE;
                }
            }
        } else {
            /* 1D or scalar - use simple sym_add for backwards compatibility */
            uint16_t size = (num_dims > 0) ? dims[0] : 1;
            uint8_t flags = (size > 1) ? (SYM_VARIABLE | SYM_ARRAY) : SYM_VARIABLE;
            if (sym_add(name, var_type, flags, size) == NULL) {
                /* May already exist */
                symbol_t *sym = sym_lookup(name);
                if (sym == NULL) {
                    set_error(p, ERR_OUT_OF_MEMORY, "Symbol table full");
                    return FALSE;
                }
            }
        }

    } while (lexer_match(&p->lex, TK_COMMA));

    return TRUE;
}

/*============================================================================
 * Formatted output helpers
 *============================================================================*/

/* Output integer with width, right-justified */
static void write_int_width(int16_t val, uint8_t width) {
    char buf[8];
    uint8_t len = 0;
    uint16_t u;
    bool_t neg = FALSE;

    if (val < 0) {
        neg = TRUE;
        u = (uint16_t)(-val);
    } else {
        u = (uint16_t)val;
    }

    /* Build digits in reverse */
    do {
        buf[len++] = '0' + (u % 10);
        u /= 10;
    } while (u > 0);

    if (neg) {
        buf[len++] = '-';
    }

    /* Pad with spaces if needed */
    uint8_t total = len;
    while (total < width) {
        acia_putc(' ');
        total++;
    }

    /* Output digits in correct order */
    while (len > 0) {
        acia_putc(buf[--len]);
    }
}


/* Parse WRITE statement: WRITE(*,*) list or WRITE(*,100) list */
bool_t parse_write(parser_t *p) {
    if (!lexer_expect(&p->lex, TK_WRITE)) {
        return FALSE;
    }

    /* Parse format specification (*,*) or (*,100) */
    if (!lexer_expect(&p->lex, TK_LPAREN)) {
        set_error(p, ERR_EXPECTED, "Expected (");
        return FALSE;
    }

    /* Skip unit number */
    if (lexer_current(&p->lex)->type == TK_STAR) {
        lexer_next(&p->lex);
    } else {
        expr_result_t unit;
        if (!parse_expression(p, &unit)) return FALSE;
    }

    if (!lexer_expect(&p->lex, TK_COMMA)) {
        set_error(p, ERR_EXPECTED, "Expected ,");
        return FALSE;
    }

    /* Parse format: * (list-directed) or label (ignored) */
    if (lexer_current(&p->lex)->type == TK_STAR) {
        lexer_next(&p->lex);
    } else {
        expr_result_t fmt_expr;
        if (!parse_expression(p, &fmt_expr)) return FALSE;
        /* Format label parsed but ignored - using list-directed output */
    }

    if (!lexer_expect(&p->lex, TK_RPAREN)) {
        set_error(p, ERR_EXPECTED, "Expected )");
        return FALSE;
    }

    /* Output items */

    do {
        token_t *tok = lexer_current(&p->lex);

        if (tok->type == TK_STRING_LIT) {
            acia_puts(tok->value.str_val);
            lexer_next(&p->lex);
        } else if (!lexer_at_eol(&p->lex)) {
            expr_result_t value;
            if (!parse_expression(p, &value)) return FALSE;

            if (value.type == TYPE_INTEGER) {
                write_int_width(value.val.i, 0);
            } else {
                bcd_print(&value.val.r);
            }
        }

        if (lexer_current(&p->lex)->type == TK_COMMA) {
            acia_putc(' ');
        }
    } while (lexer_match(&p->lex, TK_COMMA));

    acia_newline();
    return TRUE;
}

/* Parse READ statement: READ(*,*) list */
bool_t parse_read(parser_t *p) {
    if (!lexer_expect(&p->lex, TK_READ)) {
        return FALSE;
    }

    /* Skip format specification (*,*) */
    if (!lexer_expect(&p->lex, TK_LPAREN)) {
        set_error(p, ERR_EXPECTED, "Expected (");
        return FALSE;
    }

    if (lexer_current(&p->lex)->type == TK_STAR) {
        lexer_next(&p->lex);
    }

    if (!lexer_expect(&p->lex, TK_COMMA)) {
        set_error(p, ERR_EXPECTED, "Expected ,");
        return FALSE;
    }

    if (lexer_current(&p->lex)->type == TK_STAR) {
        lexer_next(&p->lex);
    }

    if (!lexer_expect(&p->lex, TK_RPAREN)) {
        set_error(p, ERR_EXPECTED, "Expected )");
        return FALSE;
    }

    /* Read variables */
    do {
        if (lexer_current(&p->lex)->type != TK_IDENT) {
            set_error(p, ERR_EXPECTED, "Expected variable");
            return FALSE;
        }

        char name[MAX_IDENT_LEN + 1];
        strcpy(name, lexer_current(&p->lex)->value.str_val);
        lexer_next(&p->lex);

        uint16_t index = 0;
        if (lexer_current(&p->lex)->type == TK_LPAREN) {
            lexer_next(&p->lex);
            expr_result_t subscript;
            if (!parse_expression(p, &subscript)) {
                return FALSE;
            }
            real_to_int(&subscript);
            index = (uint16_t)(subscript.val.i - 1);

            if (!lexer_expect(&p->lex, TK_RPAREN)) {
                set_error(p, ERR_EXPECTED, "Expected )");
                return FALSE;
            }
        }

        /* Look up or create variable */
        symbol_t *sym = sym_lookup(name);
        if (sym == NULL) {
            uint8_t type;
            if (name[0] >= 'I' && name[0] <= 'N') {
                type = TYPE_INTEGER;
            } else {
                type = TYPE_REAL;
            }
            sym = sym_add(name, type, SYM_VARIABLE, 1);
        }

        /* Read value from input */
        acia_putc('?');
        acia_putc(' ');

        char buf[32];
        uint8_t len = acia_gets(buf, 32);

        if (sym->type == TYPE_INTEGER) {
            int16_t val = 0;
            bool_t neg = FALSE;
            uint8_t i = 0;

            if (buf[0] == '-') {
                neg = TRUE;
                i = 1;
            }
            while (buf[i] >= '0' && buf[i] <= '9') {
                val = val * 10 + (buf[i] - '0');
                i++;
            }
            if (neg) val = -val;

            sym_set_int(sym, index, val);
        } else {
            bcd_t val;
            bcd_parse(&val, buf);
            sym_set_real(sym, index, &val);
        }

    } while (lexer_match(&p->lex, TK_COMMA));

    return TRUE;
}

/* Parse IF statement */
/* IF (condition) statement  -- immediate IF */
/* IF (condition) THEN       -- block IF */
bool_t parse_if(parser_t *p) {
    if (!lexer_expect(&p->lex, TK_IF)) {
        return FALSE;
    }

    if (!lexer_expect(&p->lex, TK_LPAREN)) {
        set_error(p, ERR_EXPECTED, "Expected (");
        return FALSE;
    }

    expr_result_t condition;
    if (!parse_expression(p, &condition)) {
        return FALSE;
    }

    if (!lexer_expect(&p->lex, TK_RPAREN)) {
        set_error(p, ERR_EXPECTED, "Expected )");
        return FALSE;
    }

    /* Evaluate condition */
    real_to_int(&condition);
    bool_t cond_true = (condition.val.i != 0);

    /* Check for THEN (block IF) or immediate statement */
    if (lexer_current(&p->lex)->type == TK_THEN) {
        lexer_next(&p->lex);  /* Skip THEN */

        /* Block IF - push to IF stack */
        if (!prog_push_if(cond_true)) {
            set_error(p, ERR_STACK_OVERFLOW, "IF nested too deep");
            return FALSE;
        }

        /* If condition is false, skip to ELSE or ENDIF */
        if (!cond_true) {
            prog_skip_to_else_or_endif();
            /* Set flag to not increment line (we're already at ELSE/ENDIF) */
            program.do_depth |= 0x80;
        }

        return TRUE;
    }

    /* Immediate IF: IF (cond) statement */
    if (cond_true) {
        return parse_statement(p);
    }
    /* Condition false - skip rest of line */
    return TRUE;
}

/* Parse ELSE statement */
bool_t parse_else(parser_t *p) {
    if (!lexer_expect(&p->lex, TK_ELSE)) {
        return FALSE;
    }

    /* Handle ELSE - this decides whether to execute the ELSE block */
    if (!prog_handle_else()) {
        /* prog_handle_else returns FALSE if:
           1. No matching IF (error)
           2. IF was true, so we skip to ENDIF */
        if (program.if_depth == 0) {
            set_error(p, ERR_SYNTAX, "ELSE without IF");
            return FALSE;
        }
        /* If we get here, IF was true and we skipped to ENDIF */
        /* Set flag to not increment line */
        program.do_depth |= 0x80;
    }

    return TRUE;
}

/* Parse ENDIF statement */
bool_t parse_endif(parser_t *p) {
    if (!lexer_expect(&p->lex, TK_ENDIF)) {
        return FALSE;
    }

    if (!prog_pop_if()) {
        set_error(p, ERR_SYNTAX, "ENDIF without IF");
        return FALSE;
    }

    return TRUE;
}

/*============================================================================
 * SUBROUTINE/FUNCTION/CALL/RETURN parsing
 *============================================================================*/
#ifdef FEATURE_SUBPROGRAMS
/* Full subprogram support - implementation TBD */
bool_t parse_subroutine(parser_t *p) {
    lexer_next(&p->lex);
    while (!lexer_at_eol(&p->lex)) lexer_next(&p->lex);
    if (program.running) prog_skip_subprog();
    return TRUE;
}

bool_t parse_function(parser_t *p) {
    lexer_next(&p->lex);
    while (!lexer_at_eol(&p->lex)) lexer_next(&p->lex);
    if (program.running) prog_skip_subprog();
    return TRUE;
}

bool_t parse_call(parser_t *p) {
    lexer_next(&p->lex);
    while (!lexer_at_eol(&p->lex)) lexer_next(&p->lex);
    return TRUE;
}

bool_t parse_return(parser_t *p) {
    lexer_next(&p->lex);
    return TRUE;
}
#endif /* FEATURE_SUBPROGRAMS */

/* Parse DO statement: DO label var = start, end [, step] */
bool_t parse_do(parser_t *p) {
    if (!lexer_expect(&p->lex, TK_DO)) {
        return FALSE;
    }

    /* Get the end label */
    if (lexer_current(&p->lex)->type != TK_INT_LIT) {
        set_error(p, ERR_EXPECTED, "Expected label");
        return FALSE;
    }
    uint16_t end_label = (uint16_t)lexer_current(&p->lex)->value.int_val;
    lexer_next(&p->lex);

    /* Get loop variable name */
    if (lexer_current(&p->lex)->type != TK_IDENT) {
        set_error(p, ERR_EXPECTED, "Expected variable");
        return FALSE;
    }
    char var_name[MAX_IDENT_LEN + 1];
    strcpy(var_name, lexer_current(&p->lex)->value.str_val);
    lexer_next(&p->lex);

    /* Expect = */
    if (!lexer_expect(&p->lex, TK_ASSIGN)) {
        set_error(p, ERR_EXPECTED, "Expected =");
        return FALSE;
    }

    /* Get start value */
    expr_result_t start_expr;
    if (!parse_expression(p, &start_expr)) {
        return FALSE;
    }
    real_to_int(&start_expr);
    int16_t start_val = start_expr.val.i;

    /* Expect comma */
    if (!lexer_expect(&p->lex, TK_COMMA)) {
        set_error(p, ERR_EXPECTED, "Expected ,");
        return FALSE;
    }

    /* Get end value */
    expr_result_t end_expr;
    if (!parse_expression(p, &end_expr)) {
        return FALSE;
    }
    real_to_int(&end_expr);
    int16_t end_val = end_expr.val.i;

    /* Optional step value */
    int16_t step_val = 1;
    if (lexer_current(&p->lex)->type == TK_COMMA) {
        lexer_next(&p->lex);
        expr_result_t step_expr;
        if (!parse_expression(p, &step_expr)) {
            return FALSE;
        }
        real_to_int(&step_expr);
        step_val = step_expr.val.i;
    }

    /* If running a program, set up the DO loop */
    if (program.running) {
        /* Look up or create the loop variable */
        symbol_t *sym = sym_lookup(var_name);
        if (sym == NULL) {
            sym = sym_add(var_name, TYPE_INTEGER, SYM_VARIABLE, 1);
            if (sym == NULL) {
                set_error(p, ERR_OUT_OF_MEMORY, "Symbol table full");
                return FALSE;
            }
        }

        /* Set initial value */
        sym_set_int(sym, 0, start_val);

        /* Check if loop should execute at all */
        bool_t skip_loop;
        if (step_val > 0) {
            skip_loop = (start_val > end_val);
        } else {
            skip_loop = (start_val < end_val);
        }

        if (skip_loop) {
            /* Skip to after the CONTINUE with this label */
            int8_t target = prog_find_label(end_label);
            if (target >= 0) {
                program.current_line = (uint8_t)(target + 1);
                program.do_depth |= 0x80;  /* Set GOTO flag */
            }
        } else {
            /* Push DO state onto stack */
            if (!prog_push_do(end_label, var_name, end_val, step_val)) {
                set_error(p, ERR_STACK_OVERFLOW, "DO nesting too deep");
                return FALSE;
            }
        }
    }

    return TRUE;
}

/*============================================================================
 * Parse DATA statement
 *============================================================================*/
#ifdef FEATURE_DATA
/* Full DATA implementation - TBD, currently just skips */
bool_t parse_data(parser_t *p) {
    lexer_next(&p->lex);
    while (!lexer_at_eol(&p->lex)) lexer_next(&p->lex);
    return TRUE;
}
#endif /* FEATURE_DATA */

/*============================================================================
 * Parse COMMON statement
 *============================================================================*/
#ifdef FEATURE_COMMON
/* Full COMMON implementation - TBD, currently just skips */
bool_t parse_common(parser_t *p) {
    lexer_next(&p->lex);
    while (!lexer_at_eol(&p->lex)) lexer_next(&p->lex);
    return TRUE;
}
#endif /* FEATURE_COMMON */

/*============================================================================
 * Parse FORMAT statement
 *============================================================================*/
#ifdef FEATURE_FORMAT
bool_t parse_format(parser_t *p, uint16_t label) {
    (void)label;
    lexer_next(&p->lex);
    /* Skip to closing paren */
    int depth = 0;
    while (!lexer_at_eol(&p->lex)) {
        token_t *tok = lexer_current(&p->lex);
        if (tok->type == TK_LPAREN) depth++;
        else if (tok->type == TK_RPAREN) {
            if (depth == 0) break;
            depth--;
        }
        lexer_next(&p->lex);
    }
    if (lexer_current(&p->lex)->type == TK_RPAREN) {
        lexer_next(&p->lex);
    }
    return TRUE;
}
#endif /* FEATURE_FORMAT */

/* Parse a single statement */
bool_t parse_statement(parser_t *p) {
    token_t *tok = lexer_current(&p->lex);

    /* Skip empty lines */
    if (lexer_at_eol(&p->lex)) {
        return TRUE;
    }

    /* Check for statement label */
    uint16_t stmt_label = 0;
    if (tok->type == TK_INT_LIT) {
        /* Save the label for use with CONTINUE */
        stmt_label = (uint16_t)tok->value.int_val;
        lexer_next(&p->lex);
        tok = lexer_current(&p->lex);
    }

    switch (tok->type) {
        case TK_PROGRAM:
            /* PROGRAM statement - just skip the name */
            lexer_next(&p->lex);  /* Skip PROGRAM */
            if (lexer_current(&p->lex)->type == TK_IDENT) {
                lexer_next(&p->lex);  /* Skip name */
            }
            return TRUE;

        case TK_INTEGER:
        case TK_REAL:
        case TK_DIMENSION:
            return parse_declaration(p);

#ifdef FEATURE_DATA
        case TK_DATA:
            return parse_data(p);
#endif

#ifdef FEATURE_COMMON
        case TK_COMMON:
            return parse_common(p);
#endif

#ifdef FEATURE_FORMAT
        case TK_FORMAT:
            return parse_format(p, stmt_label);
#endif

        case TK_IF:
            return parse_if(p);

        case TK_ELSE:
            return parse_else(p);

        case TK_ENDIF:
            return parse_endif(p);

        case TK_DO:
            return parse_do(p);

        case TK_WRITE:
            return parse_write(p);

        case TK_READ:
            return parse_read(p);

        case TK_GOTO:
            lexer_next(&p->lex);  /* Skip GOTO */
            if (lexer_current(&p->lex)->type == TK_INT_LIT) {
                uint16_t label = (uint16_t)lexer_current(&p->lex)->value.int_val;
                lexer_next(&p->lex);
                /* Perform GOTO only if running a program */
                if (program.running) {
                    int8_t target = prog_find_label(label);
                    if (target < 0) {
                        set_error(p, ERR_UNDEFINED_LABEL, "Undefined label");
                        return FALSE;
                    }
                    /* Set target line (run loop will NOT increment after GOTO) */
                    program.current_line = (uint8_t)target;
                    /* Use a sentinel value in current_line that run loop recognizes */
                    /* Actually, we'll modify run loop to not increment on GOTO */
                    /* For now, set a flag */
                    program.do_depth |= 0x80;  /* Use high bit as "skip increment" flag */
                }
            } else {
                set_error(p, ERR_EXPECTED, "Expected label");
                return FALSE;
            }
            return TRUE;

        case TK_STOP:
            lexer_next(&p->lex);
            if (program.running) {
                program.stopped = TRUE;
            }
            return TRUE;

#ifdef FEATURE_SUBPROGRAMS
        case TK_SUBROUTINE:
            return parse_subroutine(p);

        case TK_FUNCTION:
            return parse_function(p);

        case TK_CALL:
            return parse_call(p);

        case TK_RETURN:
            return parse_return(p);
#endif

        case TK_CONTINUE:
            lexer_next(&p->lex);
            /* If this CONTINUE has a label and we're running, check DO loops */
            if (program.running && stmt_label > 0) {
                if (!prog_pop_do(stmt_label)) {
                    /* No matching DO - that's OK, just continue */
                }
            }
            return TRUE;

        case TK_END:
            lexer_next(&p->lex);
            return TRUE;

        case TK_IDENT:
            return parse_assignment(p);

        default:
            set_error(p, ERR_SYNTAX, "Unknown statement");
            return FALSE;
    }
}
