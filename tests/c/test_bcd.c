/*
 * test_bcd.c - Unit tests for BCD floating point library
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

/* Provide the ACIA interface that bcd.c expects */
#define acia_putc(c) acia_putc(c)
#define acia_puts(s) acia_puts(s)

/* Include BCD implementation directly for host compilation */
#include "../../src/bcd.c"

/*============================================================================
 * Test: Integer to BCD conversion
 *============================================================================*/
void test_bcd_from_int_zero(void) {
    TEST("bcd_from_int(0)");
    bcd_t r;
    bcd_from_int(&r, 0);
    ASSERT_TRUE(bcd_is_zero(&r));
    ASSERT_EQ(0, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_from_int_positive(void) {
    TEST("bcd_from_int(123)");
    bcd_t r;
    bcd_from_int(&r, 123);
    ASSERT_FALSE(bcd_is_zero(&r));
    ASSERT_FALSE(bcd_is_neg(&r));
    ASSERT_EQ(123, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_from_int_negative(void) {
    TEST("bcd_from_int(-456)");
    bcd_t r;
    bcd_from_int(&r, -456);
    ASSERT_FALSE(bcd_is_zero(&r));
    ASSERT_TRUE(bcd_is_neg(&r));
    ASSERT_EQ(-456, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_from_int_max(void) {
    TEST("bcd_from_int(32767)");
    bcd_t r;
    bcd_from_int(&r, 32767);
    ASSERT_EQ(32767, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_from_int_min(void) {
    TEST("bcd_from_int(-32768)");
    bcd_t r;
    bcd_from_int(&r, -32768);
    ASSERT_EQ(-32768, bcd_to_int(&r));
    TEST_PASS();
}

/*============================================================================
 * Test: BCD parsing
 *============================================================================*/
void test_bcd_parse_integer(void) {
    TEST("bcd_parse(\"42\")");
    bcd_t r;
    uint8_t consumed = bcd_parse(&r, "42");
    ASSERT_EQ(2, consumed);
    ASSERT_EQ(42, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_parse_decimal(void) {
    TEST("bcd_parse(\"3.14\")");
    bcd_t r;
    uint8_t consumed = bcd_parse(&r, "3.14");
    ASSERT_EQ(4, consumed);
    ASSERT_EQ(3, bcd_to_int(&r));  /* Truncates to 3 */
    TEST_PASS();
}

void test_bcd_parse_negative(void) {
    TEST("bcd_parse(\"-99\")");
    bcd_t r;
    uint8_t consumed = bcd_parse(&r, "-99");
    ASSERT_EQ(3, consumed);
    ASSERT_EQ(-99, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_parse_leading_zero(void) {
    TEST("bcd_parse(\"0.5\")");
    bcd_t r;
    uint8_t consumed = bcd_parse(&r, "0.5");
    ASSERT_EQ(3, consumed);
    ASSERT_EQ(0, bcd_to_int(&r));  /* Truncates to 0 */
    TEST_PASS();
}

/*============================================================================
 * Test: BCD addition
 *============================================================================*/
void test_bcd_add_positive(void) {
    TEST("bcd_add(3 + 4)");
    bcd_t a, b, r;
    bcd_from_int(&a, 3);
    bcd_from_int(&b, 4);
    bcd_add(&r, &a, &b);
    ASSERT_EQ(7, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_add_negative(void) {
    TEST("bcd_add(10 + -3)");
    bcd_t a, b, r;
    bcd_from_int(&a, 10);
    bcd_from_int(&b, -3);
    bcd_add(&r, &a, &b);
    ASSERT_EQ(7, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_add_to_zero(void) {
    TEST("bcd_add(5 + -5)");
    bcd_t a, b, r;
    bcd_from_int(&a, 5);
    bcd_from_int(&b, -5);
    bcd_add(&r, &a, &b);
    ASSERT_TRUE(bcd_is_zero(&r));
    TEST_PASS();
}

void test_bcd_add_large(void) {
    TEST("bcd_add(10000 + 20000)");
    bcd_t a, b, r;
    bcd_from_int(&a, 10000);
    bcd_from_int(&b, 20000);
    bcd_add(&r, &a, &b);
    ASSERT_EQ(30000, bcd_to_int(&r));
    TEST_PASS();
}

/*============================================================================
 * Test: BCD subtraction
 *============================================================================*/
void test_bcd_sub_positive(void) {
    TEST("bcd_sub(10 - 3)");
    bcd_t a, b, r;
    bcd_from_int(&a, 10);
    bcd_from_int(&b, 3);
    bcd_sub(&r, &a, &b);
    ASSERT_EQ(7, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_sub_negative_result(void) {
    TEST("bcd_sub(3 - 10)");
    bcd_t a, b, r;
    bcd_from_int(&a, 3);
    bcd_from_int(&b, 10);
    bcd_sub(&r, &a, &b);
    ASSERT_EQ(-7, bcd_to_int(&r));
    TEST_PASS();
}

/*============================================================================
 * Test: BCD multiplication
 *============================================================================*/
void test_bcd_mul_positive(void) {
    TEST("bcd_mul(6 * 7)");
    bcd_t a, b, r;
    bcd_from_int(&a, 6);
    bcd_from_int(&b, 7);
    bcd_mul(&r, &a, &b);
    ASSERT_EQ(42, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_mul_by_zero(void) {
    TEST("bcd_mul(123 * 0)");
    bcd_t a, b, r;
    bcd_from_int(&a, 123);
    bcd_from_int(&b, 0);
    bcd_mul(&r, &a, &b);
    ASSERT_TRUE(bcd_is_zero(&r));
    TEST_PASS();
}

void test_bcd_mul_negative(void) {
    TEST("bcd_mul(-5 * 8)");
    bcd_t a, b, r;
    bcd_from_int(&a, -5);
    bcd_from_int(&b, 8);
    bcd_mul(&r, &a, &b);
    ASSERT_EQ(-40, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_mul_both_negative(void) {
    TEST("bcd_mul(-3 * -4)");
    bcd_t a, b, r;
    bcd_from_int(&a, -3);
    bcd_from_int(&b, -4);
    bcd_mul(&r, &a, &b);
    ASSERT_EQ(12, bcd_to_int(&r));
    TEST_PASS();
}

/*============================================================================
 * Test: BCD division
 *============================================================================*/
void test_bcd_div_exact(void) {
    TEST("bcd_div(10 / 2)");
    bcd_t a, b, r;
    bcd_from_int(&a, 10);
    bcd_from_int(&b, 2);
    ASSERT_TRUE(bcd_div(&r, &a, &b));
    ASSERT_EQ(5, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_div_by_zero(void) {
    TEST("bcd_div(10 / 0) returns FALSE");
    bcd_t a, b, r;
    bcd_from_int(&a, 10);
    bcd_from_int(&b, 0);
    ASSERT_FALSE(bcd_div(&r, &a, &b));
    TEST_PASS();
}

void test_bcd_div_negative(void) {
    TEST("bcd_div(-100 / 5)");
    bcd_t a, b, r;
    bcd_from_int(&a, -100);
    bcd_from_int(&b, 5);
    ASSERT_TRUE(bcd_div(&r, &a, &b));
    ASSERT_EQ(-20, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_div_fraction(void) {
    TEST("bcd_div(1 / 2) truncates to 0");
    bcd_t a, b, r;
    bcd_from_int(&a, 1);
    bcd_from_int(&b, 2);
    ASSERT_TRUE(bcd_div(&r, &a, &b));
    ASSERT_EQ(0, bcd_to_int(&r));  /* 0.5 truncates to 0 */
    TEST_PASS();
}

void test_bcd_div_large(void) {
    TEST("bcd_div(10000 / 100)");
    bcd_t a, b, r;
    bcd_from_int(&a, 10000);
    bcd_from_int(&b, 100);
    ASSERT_TRUE(bcd_div(&r, &a, &b));
    ASSERT_EQ(100, bcd_to_int(&r));
    TEST_PASS();
}

/*============================================================================
 * Test: BCD comparison
 *============================================================================*/
void test_bcd_cmp_equal(void) {
    TEST("bcd_cmp(42 == 42)");
    bcd_t a, b;
    bcd_from_int(&a, 42);
    bcd_from_int(&b, 42);
    ASSERT_EQ(0, bcd_cmp(&a, &b));
    TEST_PASS();
}

void test_bcd_cmp_less(void) {
    TEST("bcd_cmp(5 < 10)");
    bcd_t a, b;
    bcd_from_int(&a, 5);
    bcd_from_int(&b, 10);
    ASSERT_EQ(-1, bcd_cmp(&a, &b));
    TEST_PASS();
}

void test_bcd_cmp_greater(void) {
    TEST("bcd_cmp(100 > 50)");
    bcd_t a, b;
    bcd_from_int(&a, 100);
    bcd_from_int(&b, 50);
    ASSERT_EQ(1, bcd_cmp(&a, &b));
    TEST_PASS();
}

void test_bcd_cmp_negative(void) {
    TEST("bcd_cmp(-10 < 5)");
    bcd_t a, b;
    bcd_from_int(&a, -10);
    bcd_from_int(&b, 5);
    ASSERT_EQ(-1, bcd_cmp(&a, &b));
    TEST_PASS();
}

/*============================================================================
 * Test: BCD printing
 * Note: bcd_print uses simplified scientific notation to save code size
 *============================================================================*/
void test_bcd_print_zero(void) {
    TEST("bcd_print(0)");
    mock_acia_reset();
    bcd_t r;
    bcd_from_int(&r, 0);
    bcd_print(&r);
    ASSERT_STR_EQ("0", mock_acia_get_output());
    TEST_PASS();
}

void test_bcd_print_integer(void) {
    TEST("bcd_print(42)");
    mock_acia_reset();
    bcd_t r;
    bcd_from_int(&r, 42);
    bcd_print(&r);
    ASSERT_STR_EQ("4.20000E+01", mock_acia_get_output());
    TEST_PASS();
}

void test_bcd_print_negative(void) {
    TEST("bcd_print(-123)");
    mock_acia_reset();
    bcd_t r;
    bcd_from_int(&r, -123);
    bcd_print(&r);
    ASSERT_STR_EQ("-1.23000E+02", mock_acia_get_output());
    TEST_PASS();
}

void test_bcd_print_small_number(void) {
    TEST("bcd_print(0.005)");
    mock_acia_reset();
    bcd_t r;
    bcd_parse(&r, "0.005");
    bcd_print(&r);
    ASSERT_STR_EQ("5.00000E-03", mock_acia_get_output());
    TEST_PASS();
}

void test_bcd_print_fractional(void) {
    TEST("bcd_print(3.14159)");
    mock_acia_reset();
    bcd_t r;
    bcd_parse(&r, "3.14159");
    bcd_print(&r);
    ASSERT_STR_EQ("3.14159E+00", mock_acia_get_output());
    TEST_PASS();
}

void test_bcd_print_large_e_notation(void) {
    TEST("bcd_print large number (E notation)");
    mock_acia_reset();
    bcd_t r;
    /* Create a number > 10^8 by parsing with exponent */
    bcd_parse(&r, "1.5E10");
    bcd_print(&r);
    /* Should print in E notation */
    ASSERT_TRUE(strstr(mock_acia_get_output(), "E") != NULL);
    TEST_PASS();
}

/*============================================================================
 * Test: BCD parsing edge cases
 *============================================================================*/
void test_bcd_parse_exponent_positive(void) {
    TEST("bcd_parse(\"1.5E3\")");
    bcd_t r;
    bcd_parse(&r, "1.5E3");
    ASSERT_EQ(1500, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_parse_exponent_negative(void) {
    TEST("bcd_parse(\"5E-2\")");
    bcd_t r;
    bcd_parse(&r, "5E-2");
    /* 5E-2 = 0.05, truncates to 0 */
    ASSERT_EQ(0, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_parse_exponent_plus(void) {
    TEST("bcd_parse(\"2.5E+2\")");
    bcd_t r;
    bcd_parse(&r, "2.5E+2");
    ASSERT_EQ(250, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_parse_lowercase_e(void) {
    TEST("bcd_parse(\"1e2\")");
    bcd_t r;
    bcd_parse(&r, "1e2");
    ASSERT_EQ(100, bcd_to_int(&r));
    TEST_PASS();
}

/*============================================================================
 * Test: BCD arithmetic edge cases
 *============================================================================*/
void test_bcd_add_b_larger_exp(void) {
    TEST("bcd_add with b having larger exponent");
    bcd_t a, b, r;
    bcd_from_int(&a, 5);      /* 5 = 0.5 * 10^1 */
    bcd_from_int(&b, 1000);   /* 1000 = 0.1 * 10^4 */
    bcd_add(&r, &a, &b);
    ASSERT_EQ(1005, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_sub_b_larger_exp(void) {
    TEST("bcd_sub with b having larger exponent");
    bcd_t a, b, r;
    bcd_from_int(&a, 5);
    bcd_from_int(&b, 1000);
    bcd_sub(&r, &a, &b);
    ASSERT_EQ(-995, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_div_zero_dividend(void) {
    TEST("bcd_div(0 / 5)");
    bcd_t a, b, r;
    bcd_from_int(&a, 0);
    bcd_from_int(&b, 5);
    ASSERT_TRUE(bcd_div(&r, &a, &b));
    ASSERT_TRUE(bcd_is_zero(&r));
    TEST_PASS();
}

void test_bcd_div_repeating(void) {
    TEST("bcd_div(1 / 3) repeating decimal");
    bcd_t a, b, r;
    bcd_from_int(&a, 1);
    bcd_from_int(&b, 3);
    ASSERT_TRUE(bcd_div(&r, &a, &b));
    /* Result should be ~0.33333333 */
    ASSERT_EQ(0, bcd_to_int(&r));  /* Truncates to 0 */
    /* But first digit should be 3 */
    ASSERT_EQ(3, bcd_get_digit(&r, 0));
    TEST_PASS();
}

/*============================================================================
 * Test: BCD comparison edge cases
 *============================================================================*/
void test_bcd_cmp_both_negative(void) {
    TEST("bcd_cmp(-5 > -10)");
    bcd_t a, b;
    bcd_from_int(&a, -5);
    bcd_from_int(&b, -10);
    ASSERT_EQ(1, bcd_cmp(&a, &b));  /* -5 > -10 */
    TEST_PASS();
}

void test_bcd_cmp_both_negative_equal(void) {
    TEST("bcd_cmp(-42 == -42)");
    bcd_t a, b;
    bcd_from_int(&a, -42);
    bcd_from_int(&b, -42);
    ASSERT_EQ(0, bcd_cmp(&a, &b));
    TEST_PASS();
}

void test_bcd_cmp_zero_signs(void) {
    TEST("bcd_cmp(0 == -0)");
    bcd_t a, b;
    bcd_from_int(&a, 0);
    bcd_from_int(&b, 0);
    b.sign = BCD_SIGN_NEG;  /* Make it -0 */
    ASSERT_EQ(0, bcd_cmp(&a, &b));  /* 0 == -0 */
    TEST_PASS();
}

/*============================================================================
 * Test: BCD normalization edge cases
 *============================================================================*/
void test_bcd_normalize_leading_zeros(void) {
    TEST("bcd_normalize removes leading zeros");
    bcd_t r;
    bcd_zero(&r);
    /* Manually set 0.00123 without normalization */
    bcd_set_digit(&r, 2, 1);
    bcd_set_digit(&r, 3, 2);
    bcd_set_digit(&r, 4, 3);
    r.exp = BCD_EXP_BIAS + 3;  /* Pretend exponent */
    bcd_normalize(&r);
    /* After normalization, first digit should be 1 */
    ASSERT_EQ(1, bcd_get_digit(&r, 0));
    TEST_PASS();
}

/*============================================================================
 * Test: BCD misc edge cases
 *============================================================================*/
void test_bcd_neg_zero(void) {
    TEST("bcd_neg(0) stays positive");
    bcd_t r;
    bcd_from_int(&r, 0);
    bcd_neg(&r);
    ASSERT_EQ(BCD_SIGN_POS, r.sign);  /* Zero should stay positive */
    TEST_PASS();
}

void test_bcd_mul_large_result(void) {
    TEST("bcd_mul(1000 * 1000)");
    bcd_t a, b, r;
    bcd_from_int(&a, 1000);
    bcd_from_int(&b, 1000);
    bcd_mul(&r, &a, &b);
    /* 1000 * 1000 = 1000000 */
    /* bcd_to_int only handles up to 32767, so check is_zero instead */
    ASSERT_FALSE(bcd_is_zero(&r));
    /* First digits should be 1,0,0,0,0,0,0 */
    ASSERT_EQ(1, bcd_get_digit(&r, 0));
    TEST_PASS();
}

/*============================================================================
 * Test: BCD square root
 *============================================================================*/
#ifdef FEATURE_SQRT
void test_bcd_sqrt_zero(void) {
    TEST("bcd_sqrt(0) = 0");
    bcd_t x, r;
    bcd_from_int(&x, 0);
    bool_t result = bcd_sqrt(&r, &x);
    ASSERT_TRUE(result);
    ASSERT_TRUE(bcd_is_zero(&r));
    TEST_PASS();
}

void test_bcd_sqrt_one(void) {
    TEST("bcd_sqrt(1) = 1");
    bcd_t x, r;
    bcd_from_int(&x, 1);
    bool_t result = bcd_sqrt(&r, &x);
    ASSERT_TRUE(result);
    ASSERT_EQ(1, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_sqrt_four(void) {
    TEST("bcd_sqrt(4) = 2");
    bcd_t x, r;
    bcd_from_int(&x, 4);
    bool_t result = bcd_sqrt(&r, &x);
    ASSERT_TRUE(result);
    ASSERT_EQ(2, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_sqrt_nine(void) {
    TEST("bcd_sqrt(9) = 3");
    bcd_t x, r;
    bcd_from_int(&x, 9);
    bool_t result = bcd_sqrt(&r, &x);
    ASSERT_TRUE(result);
    ASSERT_EQ(3, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_sqrt_sixteen(void) {
    TEST("bcd_sqrt(16) = 4");
    bcd_t x, r;
    bcd_from_int(&x, 16);
    bool_t result = bcd_sqrt(&r, &x);
    ASSERT_TRUE(result);
    ASSERT_EQ(4, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_sqrt_hundred(void) {
    TEST("bcd_sqrt(100) = 10");
    bcd_t x, r;
    bcd_from_int(&x, 100);
    bool_t result = bcd_sqrt(&r, &x);
    ASSERT_TRUE(result);
    ASSERT_EQ(10, bcd_to_int(&r));
    TEST_PASS();
}

void test_bcd_sqrt_two(void) {
    TEST("bcd_sqrt(2) ≈ 1.4142135");
    bcd_t x, r;
    bcd_from_int(&x, 2);
    bool_t result = bcd_sqrt(&r, &x);
    ASSERT_TRUE(result);
    ASSERT_EQ(1, bcd_to_int(&r));  /* Truncates to 1 */
    /* Check first few digits: should be 1,4,1,4,2,1,3,5 */
    ASSERT_EQ(1, bcd_get_digit(&r, 0));
    ASSERT_EQ(4, bcd_get_digit(&r, 1));
    ASSERT_EQ(1, bcd_get_digit(&r, 2));
    ASSERT_EQ(4, bcd_get_digit(&r, 3));
    TEST_PASS();
}

void test_bcd_sqrt_negative(void) {
    TEST("bcd_sqrt(-4) returns FALSE");
    bcd_t x, r;
    bcd_from_int(&x, -4);
    bool_t result = bcd_sqrt(&r, &x);
    ASSERT_FALSE(result);  /* Cannot take sqrt of negative */
    TEST_PASS();
}

void test_bcd_sqrt_fraction(void) {
    TEST("bcd_sqrt(0.25) = 0.5");
    bcd_t x, r;
    bcd_parse(&x, "0.25");
    bool_t result = bcd_sqrt(&r, &x);
    ASSERT_TRUE(result);
    /* Result should be 0.5 */
    ASSERT_EQ(0, bcd_to_int(&r));  /* Truncates to 0 */
    ASSERT_EQ(5, bcd_get_digit(&r, 0));  /* 0.5 has first digit 5 */
    TEST_PASS();
}
#endif /* FEATURE_SQRT */

/*============================================================================
 * Main
 *============================================================================*/
int main(void) {
    printf("BCD Unit Tests\n");
    printf("==============\n\n");

    /* Conversion tests */
    test_bcd_from_int_zero();
    test_bcd_from_int_positive();
    test_bcd_from_int_negative();
    test_bcd_from_int_max();
    test_bcd_from_int_min();

    /* Parsing tests */
    test_bcd_parse_integer();
    test_bcd_parse_decimal();
    test_bcd_parse_negative();
    test_bcd_parse_leading_zero();

    /* Addition tests */
    test_bcd_add_positive();
    test_bcd_add_negative();
    test_bcd_add_to_zero();
    test_bcd_add_large();

    /* Subtraction tests */
    test_bcd_sub_positive();
    test_bcd_sub_negative_result();

    /* Multiplication tests */
    test_bcd_mul_positive();
    test_bcd_mul_by_zero();
    test_bcd_mul_negative();
    test_bcd_mul_both_negative();

    /* Division tests */
    test_bcd_div_exact();
    test_bcd_div_by_zero();
    test_bcd_div_negative();
    test_bcd_div_fraction();
    test_bcd_div_large();

    /* Comparison tests */
    test_bcd_cmp_equal();
    test_bcd_cmp_less();
    test_bcd_cmp_greater();
    test_bcd_cmp_negative();

    /* Print tests */
    test_bcd_print_zero();
    test_bcd_print_integer();
    test_bcd_print_negative();
    test_bcd_print_small_number();
    test_bcd_print_fractional();
    test_bcd_print_large_e_notation();

    /* Parsing edge cases */
    test_bcd_parse_exponent_positive();
    test_bcd_parse_exponent_negative();
    test_bcd_parse_exponent_plus();
    test_bcd_parse_lowercase_e();

    /* Arithmetic edge cases */
    test_bcd_add_b_larger_exp();
    test_bcd_sub_b_larger_exp();
    test_bcd_div_zero_dividend();
    test_bcd_div_repeating();

    /* Comparison edge cases */
    test_bcd_cmp_both_negative();
    test_bcd_cmp_both_negative_equal();
    test_bcd_cmp_zero_signs();

    /* Normalization edge cases */
    test_bcd_normalize_leading_zeros();

    /* Misc edge cases */
    test_bcd_neg_zero();
    test_bcd_mul_large_result();

    /* Square root tests */
#ifdef FEATURE_SQRT
    test_bcd_sqrt_zero();
    test_bcd_sqrt_one();
    test_bcd_sqrt_four();
    test_bcd_sqrt_nine();
    test_bcd_sqrt_sixteen();
    test_bcd_sqrt_hundred();
    test_bcd_sqrt_two();
    test_bcd_sqrt_negative();
    test_bcd_sqrt_fraction();
#endif

    TEST_SUMMARY();
    return TEST_EXIT_CODE();
}
