/*
 * test_lexer.c - Unit tests for lexer/tokenizer
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include "test_framework.h"
#include "mock_acia.h"

/* Include types first */
#include "../../include/types.h"

/* Include BCD implementation (needed by lexer for real literals) */
#include "../../src/bcd.c"

/* Include lexer implementation directly for host compilation */
#include "../../src/lexer.c"

/*============================================================================
 * Test: Basic tokens
 *============================================================================*/
void test_lexer_integer(void) {
    TEST("lexer integer literal");
    lexer_t lex;
    lexer_init(&lex, "42", 1);
    token_t *tok = lexer_current(&lex);
    ASSERT_EQ(TK_INT_LIT, tok->type);
    ASSERT_EQ(42, tok->value.int_val);
    TEST_PASS();
}

void test_lexer_negative_integer(void) {
    TEST("lexer negative integer");
    lexer_t lex;
    lexer_init(&lex, "-123", 1);
    /* Note: lexer returns MINUS and INT_LIT separately */
    token_t *tok = lexer_current(&lex);
    ASSERT_EQ(TK_MINUS, tok->type);
    lexer_next(&lex);
    tok = lexer_current(&lex);
    ASSERT_EQ(TK_INT_LIT, tok->type);
    ASSERT_EQ(123, tok->value.int_val);
    TEST_PASS();
}

void test_lexer_real(void) {
    TEST("lexer real literal");
    lexer_t lex;
    lexer_init(&lex, "3.14", 1);
    token_t *tok = lexer_current(&lex);
    ASSERT_EQ(TK_REAL_LIT, tok->type);
    /* Check mantissa starts with 3 */
    ASSERT_EQ(3, bcd_get_digit(&tok->value.real_val, 0));
    TEST_PASS();
}

void test_lexer_identifier(void) {
    TEST("lexer identifier");
    lexer_t lex;
    lexer_init(&lex, "MYVAR", 1);
    token_t *tok = lexer_current(&lex);
    ASSERT_EQ(TK_IDENT, tok->type);
    ASSERT_STR_EQ("MYVAR", tok->value.str_val);
    TEST_PASS();
}

void test_lexer_identifier_lowercase(void) {
    TEST("lexer identifier lowercase");
    lexer_t lex;
    lexer_init(&lex, "myvar", 1);
    token_t *tok = lexer_current(&lex);
    ASSERT_EQ(TK_IDENT, tok->type);
    /* Should be uppercase */
    ASSERT_STR_EQ("MYVAR", tok->value.str_val);
    TEST_PASS();
}

void test_lexer_string(void) {
    TEST("lexer string literal");
    lexer_t lex;
    lexer_init(&lex, "'HELLO'", 1);
    token_t *tok = lexer_current(&lex);
    ASSERT_EQ(TK_STRING_LIT, tok->type);
    ASSERT_STR_EQ("HELLO", tok->value.str_val);
    TEST_PASS();
}

/*============================================================================
 * Test: Keywords
 *============================================================================*/
void test_lexer_keyword_if(void) {
    TEST("lexer keyword IF");
    lexer_t lex;
    lexer_init(&lex, "IF", 1);
    ASSERT_EQ(TK_IF, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_keyword_then(void) {
    TEST("lexer keyword THEN");
    lexer_t lex;
    lexer_init(&lex, "THEN", 1);
    ASSERT_EQ(TK_THEN, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_keyword_else(void) {
    TEST("lexer keyword ELSE");
    lexer_t lex;
    lexer_init(&lex, "ELSE", 1);
    ASSERT_EQ(TK_ELSE, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_keyword_endif(void) {
    TEST("lexer keyword ENDIF");
    lexer_t lex;
    lexer_init(&lex, "ENDIF", 1);
    ASSERT_EQ(TK_ENDIF, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_keyword_do(void) {
    TEST("lexer keyword DO");
    lexer_t lex;
    lexer_init(&lex, "DO", 1);
    ASSERT_EQ(TK_DO, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_keyword_integer(void) {
    TEST("lexer keyword INTEGER");
    lexer_t lex;
    lexer_init(&lex, "INTEGER", 1);
    ASSERT_EQ(TK_INTEGER, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_keyword_real(void) {
    TEST("lexer keyword REAL");
    lexer_t lex;
    lexer_init(&lex, "REAL", 1);
    ASSERT_EQ(TK_REAL, lexer_current(&lex)->type);
    TEST_PASS();
}

/*============================================================================
 * Test: Operators
 *============================================================================*/
void test_lexer_plus(void) {
    TEST("lexer operator +");
    lexer_t lex;
    lexer_init(&lex, "+", 1);
    ASSERT_EQ(TK_PLUS, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_minus(void) {
    TEST("lexer operator -");
    lexer_t lex;
    lexer_init(&lex, "-", 1);
    ASSERT_EQ(TK_MINUS, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_star(void) {
    TEST("lexer operator *");
    lexer_t lex;
    lexer_init(&lex, "*", 1);
    ASSERT_EQ(TK_STAR, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_slash(void) {
    TEST("lexer operator /");
    lexer_t lex;
    lexer_init(&lex, "/", 1);
    ASSERT_EQ(TK_SLASH, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_power(void) {
    TEST("lexer operator **");
    lexer_t lex;
    lexer_init(&lex, "**", 1);
    ASSERT_EQ(TK_POWER, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_eq(void) {
    TEST("lexer operator .EQ.");
    lexer_t lex;
    lexer_init(&lex, ".EQ.", 1);
    ASSERT_EQ(TK_EQ, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_ne(void) {
    TEST("lexer operator .NE.");
    lexer_t lex;
    lexer_init(&lex, ".NE.", 1);
    ASSERT_EQ(TK_NE, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_lt(void) {
    TEST("lexer operator .LT.");
    lexer_t lex;
    lexer_init(&lex, ".LT.", 1);
    ASSERT_EQ(TK_LT, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_gt(void) {
    TEST("lexer operator .GT.");
    lexer_t lex;
    lexer_init(&lex, ".GT.", 1);
    ASSERT_EQ(TK_GT, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_le(void) {
    TEST("lexer operator .LE.");
    lexer_t lex;
    lexer_init(&lex, ".LE.", 1);
    ASSERT_EQ(TK_LE, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_ge(void) {
    TEST("lexer operator .GE.");
    lexer_t lex;
    lexer_init(&lex, ".GE.", 1);
    ASSERT_EQ(TK_GE, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_and(void) {
    TEST("lexer operator .AND.");
    lexer_t lex;
    lexer_init(&lex, ".AND.", 1);
    ASSERT_EQ(TK_AND, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_or(void) {
    TEST("lexer operator .OR.");
    lexer_t lex;
    lexer_init(&lex, ".OR.", 1);
    ASSERT_EQ(TK_OR, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_not(void) {
    TEST("lexer operator .NOT.");
    lexer_t lex;
    lexer_init(&lex, ".NOT.", 1);
    ASSERT_EQ(TK_NOT, lexer_current(&lex)->type);
    TEST_PASS();
}

/*============================================================================
 * Test: Punctuation
 *============================================================================*/
void test_lexer_lparen(void) {
    TEST("lexer punctuation (");
    lexer_t lex;
    lexer_init(&lex, "(", 1);
    ASSERT_EQ(TK_LPAREN, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_rparen(void) {
    TEST("lexer punctuation )");
    lexer_t lex;
    lexer_init(&lex, ")", 1);
    ASSERT_EQ(TK_RPAREN, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_comma(void) {
    TEST("lexer punctuation ,");
    lexer_t lex;
    lexer_init(&lex, ",", 1);
    ASSERT_EQ(TK_COMMA, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_assign(void) {
    TEST("lexer punctuation =");
    lexer_t lex;
    lexer_init(&lex, "=", 1);
    ASSERT_EQ(TK_ASSIGN, lexer_current(&lex)->type);
    TEST_PASS();
}

/*============================================================================
 * Test: Tokenizing sequences
 *============================================================================*/
void test_lexer_sequence(void) {
    TEST("lexer tokenize 'X = 5'");
    lexer_t lex;
    lexer_init(&lex, "X = 5", 1);

    ASSERT_EQ(TK_IDENT, lexer_current(&lex)->type);
    ASSERT_STR_EQ("X", lexer_current(&lex)->value.str_val);

    lexer_next(&lex);
    ASSERT_EQ(TK_ASSIGN, lexer_current(&lex)->type);

    lexer_next(&lex);
    ASSERT_EQ(TK_INT_LIT, lexer_current(&lex)->type);
    ASSERT_EQ(5, lexer_current(&lex)->value.int_val);

    lexer_next(&lex);
    ASSERT_TRUE(lexer_at_eol(&lex));

    TEST_PASS();
}

void test_lexer_if_statement(void) {
    TEST("lexer tokenize 'IF (X .GT. 0) THEN'");
    lexer_t lex;
    lexer_init(&lex, "IF (X .GT. 0) THEN", 1);

    ASSERT_EQ(TK_IF, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_EQ(TK_LPAREN, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_EQ(TK_IDENT, lexer_current(&lex)->type);
    ASSERT_STR_EQ("X", lexer_current(&lex)->value.str_val);
    lexer_next(&lex);
    ASSERT_EQ(TK_GT, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_EQ(TK_INT_LIT, lexer_current(&lex)->type);
    ASSERT_EQ(0, lexer_current(&lex)->value.int_val);
    lexer_next(&lex);
    ASSERT_EQ(TK_RPAREN, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_EQ(TK_THEN, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_TRUE(lexer_at_eol(&lex));

    TEST_PASS();
}

void test_lexer_do_statement(void) {
    TEST("lexer tokenize 'DO 10 I = 1, 10'");
    lexer_t lex;
    lexer_init(&lex, "DO 10 I = 1, 10", 1);

    ASSERT_EQ(TK_DO, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_EQ(TK_INT_LIT, lexer_current(&lex)->type);
    ASSERT_EQ(10, lexer_current(&lex)->value.int_val);
    lexer_next(&lex);
    ASSERT_EQ(TK_IDENT, lexer_current(&lex)->type);
    ASSERT_STR_EQ("I", lexer_current(&lex)->value.str_val);
    lexer_next(&lex);
    ASSERT_EQ(TK_ASSIGN, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_EQ(TK_INT_LIT, lexer_current(&lex)->type);
    ASSERT_EQ(1, lexer_current(&lex)->value.int_val);
    lexer_next(&lex);
    ASSERT_EQ(TK_COMMA, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_EQ(TK_INT_LIT, lexer_current(&lex)->type);
    ASSERT_EQ(10, lexer_current(&lex)->value.int_val);

    TEST_PASS();
}

void test_lexer_expression(void) {
    TEST("lexer tokenize 'A + B * C'");
    lexer_t lex;
    lexer_init(&lex, "A + B * C", 1);

    ASSERT_EQ(TK_IDENT, lexer_current(&lex)->type);
    ASSERT_STR_EQ("A", lexer_current(&lex)->value.str_val);
    lexer_next(&lex);
    ASSERT_EQ(TK_PLUS, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_EQ(TK_IDENT, lexer_current(&lex)->type);
    ASSERT_STR_EQ("B", lexer_current(&lex)->value.str_val);
    lexer_next(&lex);
    ASSERT_EQ(TK_STAR, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_EQ(TK_IDENT, lexer_current(&lex)->type);
    ASSERT_STR_EQ("C", lexer_current(&lex)->value.str_val);

    TEST_PASS();
}

/*============================================================================
 * Test: SUBROUTINE/FUNCTION keywords
 *============================================================================*/
void test_lexer_keyword_subroutine(void) {
    TEST("lexer keyword SUBROUTINE");
    lexer_t lex;
    lexer_init(&lex, "SUBROUTINE", 1);
    ASSERT_EQ(TK_SUBROUTINE, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_keyword_function(void) {
    TEST("lexer keyword FUNCTION");
    lexer_t lex;
    lexer_init(&lex, "FUNCTION", 1);
    ASSERT_EQ(TK_FUNCTION, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_keyword_call(void) {
    TEST("lexer keyword CALL");
    lexer_t lex;
    lexer_init(&lex, "CALL", 1);
    ASSERT_EQ(TK_CALL, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_keyword_return(void) {
    TEST("lexer keyword RETURN");
    lexer_t lex;
    lexer_init(&lex, "RETURN", 1);
    ASSERT_EQ(TK_RETURN, lexer_current(&lex)->type);
    TEST_PASS();
}

void test_lexer_call_statement(void) {
    TEST("lexer tokenize 'CALL MYSUB(X, Y)'");
    lexer_t lex;
    lexer_init(&lex, "CALL MYSUB(X, Y)", 1);

    ASSERT_EQ(TK_CALL, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_EQ(TK_IDENT, lexer_current(&lex)->type);
    ASSERT_STR_EQ("MYSUB", lexer_current(&lex)->value.str_val);
    lexer_next(&lex);
    ASSERT_EQ(TK_LPAREN, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_EQ(TK_IDENT, lexer_current(&lex)->type);
    ASSERT_STR_EQ("X", lexer_current(&lex)->value.str_val);
    lexer_next(&lex);
    ASSERT_EQ(TK_COMMA, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_EQ(TK_IDENT, lexer_current(&lex)->type);
    ASSERT_STR_EQ("Y", lexer_current(&lex)->value.str_val);
    lexer_next(&lex);
    ASSERT_EQ(TK_RPAREN, lexer_current(&lex)->type);

    TEST_PASS();
}

/*============================================================================
 * Test: Edge cases
 *============================================================================*/
void test_lexer_empty(void) {
    TEST("lexer empty string");
    lexer_t lex;
    lexer_init(&lex, "", 1);
    ASSERT_TRUE(lexer_at_eol(&lex));
    TEST_PASS();
}

void test_lexer_whitespace(void) {
    TEST("lexer whitespace only");
    lexer_t lex;
    lexer_init(&lex, "   ", 1);
    ASSERT_TRUE(lexer_at_eol(&lex));
    TEST_PASS();
}

void test_lexer_comment(void) {
    TEST("lexer comment");
    lexer_t lex;
    lexer_init(&lex, "X ! this is a comment", 1);
    ASSERT_EQ(TK_IDENT, lexer_current(&lex)->type);
    lexer_next(&lex);
    ASSERT_TRUE(lexer_at_eol(&lex));  /* Comment skipped */
    TEST_PASS();
}

/*============================================================================
 * Main
 *============================================================================*/
int main(void) {
    printf("Lexer Unit Tests\n");
    printf("================\n\n");

    /* Basic tokens */
    test_lexer_integer();
    test_lexer_negative_integer();
    test_lexer_real();
    test_lexer_identifier();
    test_lexer_identifier_lowercase();
    test_lexer_string();

    /* Keywords */
    test_lexer_keyword_if();
    test_lexer_keyword_then();
    test_lexer_keyword_else();
    test_lexer_keyword_endif();
    test_lexer_keyword_do();
    test_lexer_keyword_integer();
    test_lexer_keyword_real();

    /* Operators */
    test_lexer_plus();
    test_lexer_minus();
    test_lexer_star();
    test_lexer_slash();
    test_lexer_power();
    test_lexer_eq();
    test_lexer_ne();
    test_lexer_lt();
    test_lexer_gt();
    test_lexer_le();
    test_lexer_ge();
    test_lexer_and();
    test_lexer_or();
    test_lexer_not();

    /* Punctuation */
    test_lexer_lparen();
    test_lexer_rparen();
    test_lexer_comma();
    test_lexer_assign();

    /* Sequences */
    test_lexer_sequence();
    test_lexer_if_statement();
    test_lexer_do_statement();
    test_lexer_expression();

    /* SUBROUTINE/FUNCTION keywords */
    test_lexer_keyword_subroutine();
    test_lexer_keyword_function();
    test_lexer_keyword_call();
    test_lexer_keyword_return();
    test_lexer_call_statement();

    /* Edge cases */
    test_lexer_empty();
    test_lexer_whitespace();
    test_lexer_comment();

    TEST_SUMMARY();
    return TEST_EXIT_CODE();
}
