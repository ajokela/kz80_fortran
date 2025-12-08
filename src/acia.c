/*
 * MC6850 ACIA Serial Driver for RetroShield Z80
 */

#include "acia.h"

/* SDCC-specific I/O port access */
__sfr __at ACIA_CTRL acia_ctrl;
__sfr __at ACIA_DATA acia_data;

void acia_init(void) {
    /* Master reset */
    acia_ctrl = ACIA_RESET;

    /* Configure: 8N1, /16 clock divider */
    acia_ctrl = ACIA_8N1;
}

uint8_t acia_rx_ready(void) {
    return (acia_ctrl & ACIA_RDRF) != 0;
}

uint8_t acia_tx_ready(void) {
    return (acia_ctrl & ACIA_TDRE) != 0;
}

char acia_getc(void) {
    /* Wait for data available */
    while (!acia_rx_ready())
        ;
    return acia_data;
}

int acia_getc_nb(void) {
    if (acia_rx_ready()) {
        return (int)acia_data;
    }
    return -1;
}

void acia_putc(char c) {
    /* Wait for transmitter ready */
    while (!acia_tx_ready())
        ;
    acia_data = c;
}

void acia_puts(const char *s) {
    while (*s) {
        acia_putc(*s++);
    }
}

void acia_newline(void) {
    acia_putc('\r');
    acia_putc('\n');
}

uint8_t acia_gets(char *buf, uint8_t maxlen) {
    uint8_t len = 0;
    char c;

    while (len < maxlen - 1) {
        c = acia_getc();

        if (c == '\r' || c == '\n') {
            /* End of line */
            acia_newline();
            break;
        } else if (c == '\b' || c == 0x7F) {
            /* Backspace */
            if (len > 0) {
                len--;
                acia_putc('\b');
                acia_putc(' ');
                acia_putc('\b');
            }
        } else if (c >= ' ' && c < 0x7F) {
            /* Printable character */
            buf[len++] = c;
            acia_putc(c);  /* Echo */
        }
    }

    buf[len] = '\0';
    return len;
}
