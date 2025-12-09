/*
 * kz80_fortran - Fortran 77 Interpreter for RetroShield Z80
 *
 * A subset Fortran 77 interpreter written in C, cross-compiled
 * with SDCC for the Z80 processor.
 */

#include <stdint.h>
#include <string.h>
#include "acia.h"
#include "types.h"
#include "lexer.h"
#include "bcd.h"
#include "symtab.h"
#include "parser.h"
#include "program.h"

#define VERSION "0.3"

/* Input buffer */
#define INPUT_BUF_SIZE 80
static char input_buf[INPUT_BUF_SIZE];

/* Program entry mode */
static bool_t in_program_mode = FALSE;

/* Banner */
static const char banner[] =
    "FORTRAN-77 Interpreter v" VERSION "\r\n"
    "RetroShield Z80\r\n";

static const char prompt[] = "> ";
static const char prog_prompt[] = "  ";
static const char ready[] = "Ready.\r\n";

/* Print a 16-bit integer */
static void print_int(int16_t n) {
    char buf[8];
    uint8_t i = 0;
    uint16_t u;

    if (n < 0) {
        acia_putc('-');
        u = -n;
    } else {
        u = n;
    }

    /* Build digits in reverse */
    do {
        buf[i++] = '0' + (u % 10);
        u /= 10;
    } while (u > 0);

    /* Print in correct order */
    while (i > 0) {
        acia_putc(buf[--i]);
    }
}

/* Token dump for testing lexer */
static void dump_tokens(const char *line) {
    lexer_t lex;
    token_t *tok;

    lexer_init(&lex, line, 1);

    while (!lexer_at_eol(&lex)) {
        tok = lexer_current(&lex);

        acia_puts(token_name(tok->type));

        if (tok->type == TK_INT_LIT) {
            acia_putc('(');
            print_int(tok->value.int_val);
            acia_putc(')');
        } else if (tok->type == TK_IDENT) {
            acia_putc('(');
            acia_puts(tok->value.str_val);
            acia_putc(')');
        } else if (tok->type == TK_STRING_LIT) {
            acia_putc('(');
            acia_putc('\'');
            acia_puts(tok->value.str_val);
            acia_putc('\'');
            acia_putc(')');
        }

        acia_putc(' ');
        lexer_next(&lex);
    }
    acia_newline();
}

/* BCD calculator for testing */
static void bcd_calc(const char *line) {
    bcd_t a, b, r;
    char op = 0;
    uint8_t pos = 0;

    /* Skip whitespace */
    while (line[pos] == ' ') pos++;

    /* Parse first number */
    pos += bcd_parse(&a, line + pos);

    /* Skip whitespace */
    while (line[pos] == ' ') pos++;

    /* Get operator */
    op = line[pos];
    if (op == '+' || op == '-' || op == '*' || op == '/') {
        pos++;
    } else {
        /* Just print the number */
        bcd_print(&a);
        acia_newline();
        return;
    }

    /* Skip whitespace */
    while (line[pos] == ' ') pos++;

    /* Parse second number */
    bcd_parse(&b, line + pos);

    /* Perform operation */
    switch (op) {
        case '+':
            bcd_add(&r, &a, &b);
            break;
        case '-':
            bcd_sub(&r, &a, &b);
            break;
        case '*':
            bcd_mul(&r, &a, &b);
            break;
        case '/':
            if (!bcd_div(&r, &a, &b)) {
                acia_puts("Error: Division by zero");
                acia_newline();
                return;
            }
            break;
    }

    /* Print result */
    bcd_print(&r);
    acia_newline();
}

/* Expression evaluator for testing parser */
static void eval_expr(const char *line) {
    parser_t parser;
    expr_result_t result;

    parser_init(&parser, line, 1);

    if (parse_expression(&parser, &result)) {
        if (result.type == TYPE_INTEGER) {
            print_int(result.val.i);
        } else {
            bcd_print(&result.val.r);
        }
        acia_newline();
    } else {
        acia_puts("Error: ");
        acia_puts(parser_error_msg(&parser));
        acia_newline();
    }
}

/* Extract statement label from line, if any */
static uint16_t extract_label(const char *line) {
    uint16_t label = 0;
    uint8_t i = 0;

    /* Skip whitespace */
    while (line[i] == ' ') i++;

    /* Check for digit */
    if (line[i] >= '0' && line[i] <= '9') {
        while (line[i] >= '0' && line[i] <= '9') {
            label = label * 10 + (line[i] - '0');
            i++;
        }
        /* Must be followed by whitespace to be a label */
        if (line[i] == ' ' || line[i] == '\0' || line[i] == '\t') {
            return label;
        }
    }
    return 0;
}

/* Check if line starts with keyword (case-insensitive) */
static bool_t starts_with(const char *line, const char *kw) {
    uint8_t i = 0;
    /* Skip whitespace */
    while (line[i] == ' ') i++;

    /* Compare */
    while (*kw) {
        char c = line[i];
        if (c >= 'a' && c <= 'z') c = c - 'a' + 'A';
        if (c != *kw) return FALSE;
        i++;
        kw++;
    }
    /* Check next char is not alphanumeric */
    char c = line[i];
    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
        (c >= '0' && c <= '9')) return FALSE;
    return TRUE;
}

/* Process input line */
static void process_line(const char *line) {
    parser_t parser;

    /* Check for program start */
    if (starts_with(line, "PROGRAM")) {
        prog_init();
        in_program_mode = TRUE;
        /* Add PROGRAM line */
        prog_add_line(line, 0);
        return;
    }

    /* In program mode, collect lines */
    if (in_program_mode) {
        uint16_t label = extract_label(line);

        /* Check for END */
        if (starts_with(line, "END")) {
            prog_add_line(line, label);
            in_program_mode = FALSE;
            acia_puts("Program entered. Type RUN to execute.\r\n");
            return;
        }

        /* Add line to program */
        if (!prog_add_line(line, label)) {
            acia_puts("Error: Program too long\r\n");
            in_program_mode = FALSE;
        }
        return;
    }

    /* Commands */
    if (starts_with(line, "RUN")) {
        if (program.num_lines == 0) {
            acia_puts("No program.\r\n");
        } else {
            prog_run();
        }
        return;
    }

    if (starts_with(line, "NEW")) {
        prog_init();
        sym_init();
        acia_puts("Ready.\r\n");
        return;
    }

    if (starts_with(line, "LIST")) {
        for (uint8_t i = 0; i < program.num_lines; i++) {
            const char *text = prog_lines[i].text;
            acia_puts(text);
            acia_newline();
        }
        return;
    }

    /* Immediate mode - execute statement */
    parser_init(&parser, line, 1);

    if (!parse_statement(&parser)) {
        acia_puts("Error: ");
        acia_puts(parser_error_msg(&parser));
        acia_newline();
    }
}

void main(void) {
    /* Initialize ACIA */
    acia_init();

    /* Initialize program mode flag */
    in_program_mode = FALSE;

    /* Initialize symbol table (full clear at startup) */
    sym_init_all();

    /* Initialize program */
    prog_init();

    /* Print banner */
    acia_puts(banner);
    acia_puts(ready);

    /* Main REPL loop */
    while (1) {
        if (in_program_mode) {
            acia_puts(prog_prompt);
        } else {
            acia_puts(prompt);
        }

        /* Read a line */
        uint8_t len = acia_gets(input_buf, INPUT_BUF_SIZE);

        if (len > 0) {
            process_line(input_buf);
        }
    }
}
