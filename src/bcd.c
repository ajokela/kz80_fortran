/*
 * bcd.c - BCD Floating Point Library for kz80_fortran
 */

#include "config.h"
#include "bcd.h"
#include "acia.h"
#include <string.h>

/*============================================================================
 * Constants
 *============================================================================*/
const bcd_t BCD_ZERO = { BCD_SIGN_POS, BCD_EXP_BIAS, { 0x00, 0x00, 0x00, 0x00 } };
const bcd_t BCD_ONE  = { BCD_SIGN_POS, BCD_EXP_BIAS + 1, { 0x10, 0x00, 0x00, 0x00 } };
const bcd_t BCD_TEN  = { BCD_SIGN_POS, BCD_EXP_BIAS + 2, { 0x10, 0x00, 0x00, 0x00 } };

/*============================================================================
 * Digit access helpers
 *============================================================================*/
uint8_t bcd_get_digit(const bcd_t *a, uint8_t pos) {
    uint8_t byte_idx = pos / 2;
    if (pos & 1) {
        return a->digits[byte_idx] & 0x0F;
    } else {
        return (a->digits[byte_idx] >> 4) & 0x0F;
    }
}

void bcd_set_digit(bcd_t *r, uint8_t pos, uint8_t val) {
    uint8_t byte_idx = pos / 2;
    if (pos & 1) {
        r->digits[byte_idx] = (r->digits[byte_idx] & 0xF0) | (val & 0x0F);
    } else {
        r->digits[byte_idx] = (r->digits[byte_idx] & 0x0F) | ((val & 0x0F) << 4);
    }
}

/*============================================================================
 * Basic operations
 *============================================================================*/
void bcd_zero(bcd_t *r) {
    r->sign = BCD_SIGN_POS;
    r->exp = BCD_EXP_BIAS;
    memset(r->digits, 0, 4);
}

void bcd_copy(bcd_t *dest, const bcd_t *src) {
    memcpy(dest, src, sizeof(bcd_t));
}

void bcd_neg(bcd_t *r) {
    if (!bcd_is_zero(r)) {
        r->sign ^= BCD_SIGN_NEG;
    }
}

void bcd_abs(bcd_t *r, const bcd_t *a) {
    bcd_copy(r, a);
    r->sign = BCD_SIGN_POS;
}

bool_t bcd_is_zero(const bcd_t *a) {
    return (a->digits[0] | a->digits[1] | a->digits[2] | a->digits[3]) == 0;
}

bool_t bcd_is_neg(const bcd_t *a) {
    return a->sign == BCD_SIGN_NEG && !bcd_is_zero(a);
}

/*============================================================================
 * Normalization
 *============================================================================*/
void bcd_normalize(bcd_t *r) {
    /* Shift left until first digit is non-zero or all zeros */
    while (bcd_get_digit(r, 0) == 0 && !bcd_is_zero(r)) {
        /* Shift all digits left by 1 */
        for (uint8_t i = 0; i < 7; i++) {
            bcd_set_digit(r, i, bcd_get_digit(r, i + 1));
        }
        bcd_set_digit(r, 7, 0);
        r->exp--;
    }

    /* If zero, normalize exponent */
    if (bcd_is_zero(r)) {
        r->sign = BCD_SIGN_POS;
        r->exp = BCD_EXP_BIAS;
    }
}

/*============================================================================
 * Integer conversion
 *============================================================================*/
void bcd_from_int(bcd_t *r, int16_t n) {
    uint16_t u;

    bcd_zero(r);

    if (n == 0) return;

    if (n < 0) {
        r->sign = BCD_SIGN_NEG;
        u = -n;
    } else {
        u = n;
    }

    /* Count digits and fill from left */
    uint8_t digits[8];
    uint8_t num_digits = 0;

    while (u > 0 && num_digits < 8) {
        digits[num_digits++] = u % 10;
        u /= 10;
    }

    /* Copy digits to BCD in correct order (most significant first) */
    for (uint8_t i = 0; i < num_digits; i++) {
        bcd_set_digit(r, i, digits[num_digits - 1 - i]);
    }

    /* Exponent = number of digits (since mantissa is 0.xxxx) */
    r->exp = BCD_EXP_BIAS + num_digits;
}

int16_t bcd_to_int(const bcd_t *a) {
    int16_t result = 0;
    int8_t exp = (int8_t)(a->exp - BCD_EXP_BIAS);

    /* Get integer part */
    for (uint8_t i = 0; i < 8 && exp > 0; i++, exp--) {
        result = result * 10 + bcd_get_digit(a, i);
    }

    if (a->sign == BCD_SIGN_NEG) {
        result = -result;
    }

    return result;
}

/*============================================================================
 * Comparison
 *============================================================================*/
int8_t bcd_cmp(const bcd_t *a, const bcd_t *b) {
    /* Handle signs */
    if (a->sign != b->sign) {
        if (bcd_is_zero(a) && bcd_is_zero(b)) return 0;
        return (a->sign == BCD_SIGN_NEG) ? -1 : 1;
    }

    /* Both same sign - compare magnitudes */
    int8_t result;

    if (a->exp != b->exp) {
        result = (a->exp > b->exp) ? 1 : -1;
    } else {
        /* Compare mantissas */
        result = 0;
        for (uint8_t i = 0; i < 4 && result == 0; i++) {
            if (a->digits[i] > b->digits[i]) result = 1;
            else if (a->digits[i] < b->digits[i]) result = -1;
        }
    }

    /* Flip result for negative numbers */
    if (a->sign == BCD_SIGN_NEG) {
        result = -result;
    }

    return result;
}

/*============================================================================
 * Addition (internal, assumes same sign)
 *============================================================================*/
static void bcd_add_unsigned(bcd_t *r, const bcd_t *a, const bcd_t *b) {
    uint8_t work[9];  /* 9 digits for overflow */
    int8_t exp_a = a->exp;
    int8_t exp_b = b->exp;
    int8_t exp_r;

    memset(work, 0, 9);

    /* Align to larger exponent */
    if (exp_a >= exp_b) {
        exp_r = exp_a;
        int8_t shift = exp_a - exp_b;

        /* Add a */
        for (uint8_t i = 0; i < 8; i++) {
            work[i + 1] += bcd_get_digit(a, i);
        }

        /* Add b (shifted) */
        for (uint8_t i = 0; i < 8 && i + shift < 9; i++) {
            work[i + 1 + shift] += bcd_get_digit(b, i);
        }
    } else {
        exp_r = exp_b;
        int8_t shift = exp_b - exp_a;

        /* Add b */
        for (uint8_t i = 0; i < 8; i++) {
            work[i + 1] += bcd_get_digit(b, i);
        }

        /* Add a (shifted) */
        for (uint8_t i = 0; i < 8 && i + shift < 9; i++) {
            work[i + 1 + shift] += bcd_get_digit(a, i);
        }
    }

    /* Propagate carries */
    for (int8_t i = 8; i >= 0; i--) {
        if (work[i] >= 10) {
            work[i - 1] += work[i] / 10;
            work[i] %= 10;
        }
    }

    /* Handle overflow (work[0] != 0) */
    if (work[0] != 0) {
        for (uint8_t i = 0; i < 8; i++) {
            bcd_set_digit(r, i, work[i]);
        }
        exp_r++;
    } else {
        for (uint8_t i = 0; i < 8; i++) {
            bcd_set_digit(r, i, work[i + 1]);
        }
    }

    r->exp = exp_r;
    bcd_normalize(r);
}

/*============================================================================
 * Subtraction (internal, assumes a >= b and same sign)
 *============================================================================*/
static void bcd_sub_unsigned(bcd_t *r, const bcd_t *a, const bcd_t *b) {
    uint8_t work[8];
    int8_t exp_a = a->exp;
    int8_t exp_b = b->exp;
    int8_t shift = exp_a - exp_b;

    /* Copy a to work */
    for (uint8_t i = 0; i < 8; i++) {
        work[i] = bcd_get_digit(a, i);
    }

    /* Subtract b (shifted) */
    for (uint8_t i = 0; i < 8 && i + shift < 8; i++) {
        int8_t idx = i + shift;
        uint8_t bdig = bcd_get_digit(b, i);

        if (work[idx] >= bdig) {
            work[idx] -= bdig;
        } else {
            /* Borrow */
            work[idx] += 10 - bdig;
            for (int8_t j = idx - 1; j >= 0; j--) {
                if (work[j] > 0) {
                    work[j]--;
                    break;
                }
                work[j] = 9;
            }
        }
    }

    /* Store result */
    for (uint8_t i = 0; i < 8; i++) {
        bcd_set_digit(r, i, work[i]);
    }
    r->exp = exp_a;

    bcd_normalize(r);
}

/*============================================================================
 * Public addition
 *============================================================================*/
void bcd_add(bcd_t *r, const bcd_t *a, const bcd_t *b) {
    if (bcd_is_zero(a)) {
        bcd_copy(r, b);
        return;
    }
    if (bcd_is_zero(b)) {
        bcd_copy(r, a);
        return;
    }

    if (a->sign == b->sign) {
        /* Same sign: add magnitudes */
        r->sign = a->sign;
        bcd_add_unsigned(r, a, b);
    } else {
        /* Different signs: subtract smaller from larger */
        bcd_t abs_a, abs_b;
        bcd_abs(&abs_a, a);
        bcd_abs(&abs_b, b);

        int8_t cmp = bcd_cmp(&abs_a, &abs_b);

        if (cmp == 0) {
            bcd_zero(r);
        } else if (cmp > 0) {
            r->sign = a->sign;
            bcd_sub_unsigned(r, &abs_a, &abs_b);
        } else {
            r->sign = b->sign;
            bcd_sub_unsigned(r, &abs_b, &abs_a);
        }
    }
}

/*============================================================================
 * Public subtraction
 *============================================================================*/
void bcd_sub(bcd_t *r, const bcd_t *a, const bcd_t *b) {
    bcd_t neg_b;
    bcd_copy(&neg_b, b);
    bcd_neg(&neg_b);
    bcd_add(r, a, &neg_b);
}

/*============================================================================
 * Multiplication
 *============================================================================*/
void bcd_mul(bcd_t *r, const bcd_t *a, const bcd_t *b) {
    uint8_t work[16];
    memset(work, 0, 16);

    if (bcd_is_zero(a) || bcd_is_zero(b)) {
        bcd_zero(r);
        return;
    }

    /* Multiply each digit pair */
    for (uint8_t i = 0; i < 8; i++) {
        uint8_t da = bcd_get_digit(a, i);
        for (uint8_t j = 0; j < 8; j++) {
            uint8_t db = bcd_get_digit(b, j);
            uint16_t prod = (uint16_t)da * (uint16_t)db;
            work[i + j] += (uint8_t)(prod / 10);
            work[i + j + 1] += (uint8_t)(prod % 10);
        }
    }

    /* Propagate carries (multiple passes for large carries) */
    for (uint8_t pass = 0; pass < 3; pass++) {
        for (int8_t i = 15; i > 0; i--) {
            if (work[i] >= 10) {
                work[i - 1] += work[i] / 10;
                work[i] %= 10;
            }
        }
    }

    /* Determine result exponent: exp_a + exp_b - bias (since both already biased) */
    int16_t exp_r = (int16_t)a->exp + (int16_t)b->exp - BCD_EXP_BIAS;

    /* Find first non-zero digit */
    uint8_t start = 0;
    while (start < 16 && work[start] == 0) start++;

    if (start >= 16) {
        bcd_zero(r);
        return;
    }

    /* Adjust exponent: result mantissa starts at position 'start' */
    /* Original mantissas are 0.xxxxx, product is at position start in work array */
    exp_r = exp_r - start;

    /* Copy 8 digits to result */
    for (uint8_t i = 0; i < 8; i++) {
        bcd_set_digit(r, i, (start + i < 16) ? work[start + i] : 0);
    }

    r->sign = (a->sign == b->sign) ? BCD_SIGN_POS : BCD_SIGN_NEG;
    r->exp = (uint8_t)exp_r;

    bcd_normalize(r);
}

/*============================================================================
 * Division helper: compare 8-digit arrays, return 1 if a>b, -1 if a<b, 0 if equal
 *============================================================================*/
static int8_t cmp_digits(const uint8_t *a, const uint8_t *b) {
    for (uint8_t i = 0; i < 8; i++) {
        if (a[i] > b[i]) return 1;
        if (a[i] < b[i]) return -1;
    }
    return 0;
}

/*============================================================================
 * Division helper: subtract b from a (8 digits), return borrow
 *============================================================================*/
static uint8_t sub_digits(uint8_t *a, const uint8_t *b) {
    int8_t borrow = 0;
    for (int8_t i = 7; i >= 0; i--) {
        int8_t diff = (int8_t)a[i] - (int8_t)b[i] - borrow;
        if (diff < 0) {
            diff += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }
        a[i] = (uint8_t)diff;
    }
    return borrow;
}

/*============================================================================
 * Division - using repeated subtraction with proper digit extraction
 *
 * Algorithm:
 * 1. Start with dividend mantissa in accumulator
 * 2. For each quotient digit:
 *    a. Count how many times divisor fits into accumulator
 *    b. Subtract that many times
 *    c. Shift accumulator left (multiply by 10)
 * 3. Normalize result if first digit is 0
 *============================================================================*/
bool_t bcd_div(bcd_t *r, const bcd_t *a, const bcd_t *b) {
    if (bcd_is_zero(b)) {
        return FALSE;  /* Division by zero */
    }

    if (bcd_is_zero(a)) {
        bcd_zero(r);
        return TRUE;
    }

    /* Working accumulator - 9 digits to handle overflow during compare */
    uint8_t acc[9];
    uint8_t divisor[9];
    uint8_t quot[9];

    memset(acc, 0, 9);
    memset(divisor, 0, 9);
    memset(quot, 0, 9);

    /* Copy dividend mantissa to accumulator (positions 1-8, leave pos 0 for overflow) */
    for (uint8_t i = 0; i < 8; i++) {
        acc[i + 1] = bcd_get_digit(a, i);
    }

    /* Copy divisor mantissa (positions 1-8) */
    for (uint8_t i = 0; i < 8; i++) {
        divisor[i + 1] = bcd_get_digit(b, i);
    }

    /* Calculate result exponent: exp_a - exp_b + bias + 1 (we'll adjust if needed) */
    int16_t exp_r = (int16_t)a->exp - (int16_t)b->exp + BCD_EXP_BIAS + 1;

    /* Generate 9 quotient digits (we may need to discard the first if it's 0) */
    for (uint8_t q = 0; q < 9; q++) {
        /* Count how many times divisor fits into accumulator */
        uint8_t count = 0;

        while (count < 10) {
            /* Compare acc with divisor (9 digits) */
            int8_t cmp = 0;
            for (uint8_t i = 0; i < 9; i++) {
                if (acc[i] > divisor[i]) { cmp = 1; break; }
                if (acc[i] < divisor[i]) { cmp = -1; break; }
            }

            if (cmp < 0) break;  /* divisor > acc, we're done */

            /* Subtract divisor from acc */
            int8_t borrow = 0;
            for (int8_t i = 8; i >= 0; i--) {
                int8_t diff = (int8_t)acc[i] - (int8_t)divisor[i] - borrow;
                if (diff < 0) {
                    diff += 10;
                    borrow = 1;
                } else {
                    borrow = 0;
                }
                acc[i] = (uint8_t)diff;
            }
            count++;
        }

        quot[q] = count;

        /* Shift accumulator left by 1 digit (multiply remainder by 10) */
        for (uint8_t i = 0; i < 8; i++) {
            acc[i] = acc[i + 1];
        }
        acc[8] = 0;
    }

    /* Find first non-zero quotient digit */
    uint8_t start = 0;
    while (start < 8 && quot[start] == 0) {
        start++;
        exp_r--;
    }

    /* Store result (8 digits starting from first non-zero) */
    for (uint8_t i = 0; i < 8; i++) {
        if (start + i < 9) {
            bcd_set_digit(r, i, quot[start + i]);
        } else {
            bcd_set_digit(r, i, 0);
        }
    }

    r->sign = (a->sign == b->sign) ? BCD_SIGN_POS : BCD_SIGN_NEG;

    /* Handle exponent overflow/underflow */
    if (exp_r < 0) exp_r = 0;
    if (exp_r > 127) exp_r = 127;
    r->exp = (uint8_t)exp_r;

    bcd_normalize(r);
    return TRUE;
}

/*============================================================================
 * Square root - Newton-Raphson method
 * x_{n+1} = (x_n + S/x_n) / 2
 *============================================================================*/
#ifdef FEATURE_SQRT
bool_t bcd_sqrt(bcd_t *r, const bcd_t *x) {
    /* Cannot take sqrt of negative number */
    if (bcd_is_neg(x)) {
        return FALSE;
    }

    /* sqrt(0) = 0 */
    if (bcd_is_zero(x)) {
        bcd_zero(r);
        return TRUE;
    }

    bcd_t guess, prev, two, temp;

    /* Initialize two = 2 */
    bcd_from_int(&two, 2);

    /* Initial guess: use x itself, or for better convergence,
     * adjust exponent to get a reasonable starting point.
     * For x = m * 10^e, sqrt(x) ≈ sqrt(m) * 10^(e/2)
     */
    bcd_copy(&guess, x);

    /* Adjust initial guess based on exponent for faster convergence */
    int8_t exp = (int8_t)(x->exp - BCD_EXP_BIAS);
    if (exp > 2) {
        /* Start with a smaller guess for large numbers */
        guess.exp = BCD_EXP_BIAS + (exp / 2) + 1;
    } else if (exp < -2) {
        /* Start with a larger guess for small numbers */
        guess.exp = BCD_EXP_BIAS + (exp / 2);
    }

    /* Newton-Raphson iteration: guess = (guess + x/guess) / 2
     * Typically converges in 8-12 iterations for 8-digit precision */
    for (uint8_t i = 0; i < 15; i++) {
        bcd_copy(&prev, &guess);

        /* temp = x / guess */
        if (!bcd_div(&temp, x, &guess)) {
            /* Division failed, return current guess */
            break;
        }

        /* guess = guess + temp */
        bcd_add(&guess, &guess, &temp);

        /* guess = guess / 2 */
        bcd_div(&guess, &guess, &two);

        /* Check for convergence (guess == prev) */
        if (bcd_cmp(&guess, &prev) == 0) {
            break;
        }
    }

    bcd_copy(r, &guess);
    return TRUE;
}
#endif /* FEATURE_SQRT */

/*============================================================================
 * Parsing
 *============================================================================*/
uint8_t bcd_parse(bcd_t *r, const char *s) {
    uint8_t pos = 0;
    uint8_t digits[8];
    uint8_t num_digits = 0;
    int8_t decimal_pos = -1;
    int8_t exp_adjust = 0;
    bool_t negative = FALSE;

    memset(digits, 0, 8);
    bcd_zero(r);

    /* Skip whitespace */
    while (s[pos] == ' ') pos++;

    /* Sign */
    if (s[pos] == '-') {
        negative = TRUE;
        pos++;
    } else if (s[pos] == '+') {
        pos++;
    }

    /* Integer and fractional parts */
    while ((s[pos] >= '0' && s[pos] <= '9') || s[pos] == '.') {
        if (s[pos] == '.') {
            if (decimal_pos >= 0) break;  /* Second decimal */
            decimal_pos = num_digits;
        } else if (num_digits < 8) {
            digits[num_digits++] = s[pos] - '0';
        }
        pos++;
    }

    /* Exponent */
    if (s[pos] == 'E' || s[pos] == 'e') {
        pos++;
        bool_t exp_neg = FALSE;
        int16_t exp_val = 0;

        if (s[pos] == '-') {
            exp_neg = TRUE;
            pos++;
        } else if (s[pos] == '+') {
            pos++;
        }

        while (s[pos] >= '0' && s[pos] <= '9') {
            exp_val = exp_val * 10 + (s[pos] - '0');
            pos++;
        }

        if (exp_neg) exp_val = -exp_val;
        exp_adjust = (int8_t)exp_val;
    }

    if (num_digits == 0) {
        return pos;  /* No digits found */
    }

    /* Calculate exponent */
    int8_t exp;
    if (decimal_pos < 0) {
        exp = num_digits + exp_adjust;
    } else {
        exp = decimal_pos + exp_adjust;
    }

    /* Store digits */
    for (uint8_t i = 0; i < 8; i++) {
        bcd_set_digit(r, i, (i < num_digits) ? digits[i] : 0);
    }

    r->sign = negative ? BCD_SIGN_NEG : BCD_SIGN_POS;
    r->exp = BCD_EXP_BIAS + exp;

    bcd_normalize(r);
    return pos;
}

/*============================================================================
 * Printing - simplified version
 *============================================================================*/
void bcd_print(const bcd_t *a) {
    if (bcd_is_zero(a)) { acia_putc('0'); return; }
    if (a->sign == BCD_SIGN_NEG) acia_putc('-');
    int8_t exp = (int8_t)(a->exp - BCD_EXP_BIAS);

    /* Always use scientific notation for simplicity */
    acia_putc('0' + bcd_get_digit(a, 0));
    acia_putc('.');
    for (uint8_t i = 1; i < 6; i++) {
        acia_putc('0' + bcd_get_digit(a, i));
    }
    acia_putc('E');
    int8_t e = exp - 1;
    if (e < 0) { acia_putc('-'); e = -e; } else acia_putc('+');
    acia_putc('0' + e / 10);
    acia_putc('0' + e % 10);
}
