/*
 * bcd.h - BCD Floating Point Library for kz80_fortran
 *
 * 6-byte BCD floating point format:
 *   Byte 0: Sign (0x00 = positive, 0x80 = negative)
 *   Byte 1: Exponent (biased by 0x40, so 0x40 = 10^0)
 *   Bytes 2-5: 8 BCD digits (mantissa, packed 2 per byte)
 *
 * The mantissa is normalized so that the first digit is non-zero
 * (except for zero itself). Value = mantissa * 10^(exp - 0x40)
 */

#ifndef BCD_H
#define BCD_H

#include "config.h"
#include "types.h"

/*============================================================================
 * BCD Constants
 *============================================================================*/
extern const bcd_t BCD_ZERO;
extern const bcd_t BCD_ONE;
extern const bcd_t BCD_TEN;

/*============================================================================
 * Initialization / Conversion
 *============================================================================*/

/* Set BCD to zero */
void bcd_zero(bcd_t *r);

/* Convert 16-bit signed integer to BCD */
void bcd_from_int(bcd_t *r, int16_t n);

/* Convert BCD to 16-bit signed integer (truncates) */
int16_t bcd_to_int(const bcd_t *a);

/* Copy BCD value */
void bcd_copy(bcd_t *dest, const bcd_t *src);

/* Negate BCD value */
void bcd_neg(bcd_t *r);

/* Get absolute value */
void bcd_abs(bcd_t *r, const bcd_t *a);

/*============================================================================
 * Arithmetic Operations
 *============================================================================*/

/* r = a + b */
void bcd_add(bcd_t *r, const bcd_t *a, const bcd_t *b);

/* r = a - b */
void bcd_sub(bcd_t *r, const bcd_t *a, const bcd_t *b);

/* r = a * b */
void bcd_mul(bcd_t *r, const bcd_t *a, const bcd_t *b);

/* r = a / b (returns FALSE if division by zero) */
bool_t bcd_div(bcd_t *r, const bcd_t *a, const bcd_t *b);

#ifdef FEATURE_SQRT
/* r = sqrt(x) (returns FALSE if x is negative) */
bool_t bcd_sqrt(bcd_t *r, const bcd_t *x);
#endif

/*============================================================================
 * Comparison
 *============================================================================*/

/* Compare a and b: returns -1 if a<b, 0 if a==b, 1 if a>b */
int8_t bcd_cmp(const bcd_t *a, const bcd_t *b);

/* Check if zero */
bool_t bcd_is_zero(const bcd_t *a);

/* Check if negative */
bool_t bcd_is_neg(const bcd_t *a);

/*============================================================================
 * I/O
 *============================================================================*/

/* Parse BCD from string, returns number of characters consumed */
uint8_t bcd_parse(bcd_t *r, const char *s);

/* Print BCD to output (via acia_putc) */
void bcd_print(const bcd_t *a);

/*============================================================================
 * Internal helpers (exposed for testing)
 *============================================================================*/

/* Normalize mantissa (shift left until first digit is non-zero) */
void bcd_normalize(bcd_t *r);

/* Get digit at position (0 = leftmost, 7 = rightmost) */
uint8_t bcd_get_digit(const bcd_t *a, uint8_t pos);

/* Set digit at position */
void bcd_set_digit(bcd_t *r, uint8_t pos, uint8_t val);

#endif /* BCD_H */
