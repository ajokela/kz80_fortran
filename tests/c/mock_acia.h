/*
 * mock_acia.h - Mock ACIA functions for host testing
 */

#ifndef MOCK_ACIA_H
#define MOCK_ACIA_H

#include <stdio.h>
#include <string.h>

/* Output buffer for capturing ACIA output */
static char acia_output[1024];
static int acia_output_pos = 0;

/* Input buffer for simulating ACIA input */
static const char *acia_input = NULL;
static int acia_input_pos = 0;

/* Reset buffers */
static void mock_acia_reset(void) {
    acia_output_pos = 0;
    acia_output[0] = '\0';
    acia_input = NULL;
    acia_input_pos = 0;
}

/* Set input for testing */
static void mock_acia_set_input(const char *input) {
    acia_input = input;
    acia_input_pos = 0;
}

/* Get captured output */
static const char *mock_acia_get_output(void) {
    return acia_output;
}

/* Mock ACIA functions */
static void acia_init(void) { }

static uint8_t acia_rx_ready(void) {
    return (acia_input && acia_input[acia_input_pos] != '\0') ? 1 : 0;
}

static uint8_t acia_tx_ready(void) {
    return 1;
}

static char acia_getc(void) {
    if (acia_input && acia_input[acia_input_pos] != '\0') {
        return acia_input[acia_input_pos++];
    }
    return '\0';
}

static void acia_putc(char c) {
    if (acia_output_pos < (int)sizeof(acia_output) - 1) {
        acia_output[acia_output_pos++] = c;
        acia_output[acia_output_pos] = '\0';
    }
}

static void acia_puts(const char *s) {
    while (*s) {
        acia_putc(*s++);
    }
}

static void acia_newline(void) {
    acia_putc('\r');
    acia_putc('\n');
}

#endif /* MOCK_ACIA_H */
