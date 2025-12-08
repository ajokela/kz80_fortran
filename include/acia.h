/*
 * MC6850 ACIA Serial Driver for RetroShield Z80
 *
 * I/O Ports:
 *   $80 - Control/Status register
 *   $81 - Data register
 */

#ifndef ACIA_H
#define ACIA_H

#include <stdint.h>

/* ACIA I/O port addresses */
#define ACIA_CTRL   0x80
#define ACIA_DATA   0x81

/* Status register bits (active high) */
#define ACIA_RDRF   0x01    /* Receive Data Register Full */
#define ACIA_TDRE   0x02    /* Transmit Data Register Empty */
#define ACIA_DCD    0x04    /* Data Carrier Detect */
#define ACIA_CTS    0x08    /* Clear To Send */
#define ACIA_FE     0x10    /* Framing Error */
#define ACIA_OVRN   0x20    /* Receiver Overrun */
#define ACIA_PE     0x40    /* Parity Error */
#define ACIA_IRQ    0x80    /* Interrupt Request */

/* Control register values */
#define ACIA_RESET  0x03    /* Master reset */
#define ACIA_8N1    0x15    /* 8 data bits, 1 stop bit, no parity, /16 clock */

/* Initialize the ACIA */
void acia_init(void);

/* Check if a character is available to read */
uint8_t acia_rx_ready(void);

/* Check if transmitter is ready */
uint8_t acia_tx_ready(void);

/* Read a character (blocking) */
char acia_getc(void);

/* Read a character if available (non-blocking), returns -1 if none */
int acia_getc_nb(void);

/* Write a character (blocking) */
void acia_putc(char c);

/* Write a null-terminated string */
void acia_puts(const char *s);

/* Write a newline (CR+LF) */
void acia_newline(void);

/* Read a line into buffer (with echo), returns length */
uint8_t acia_gets(char *buf, uint8_t maxlen);

#endif /* ACIA_H */
