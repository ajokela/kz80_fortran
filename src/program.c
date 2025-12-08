/*
 * program.c - Program storage and execution for kz80_fortran
 */

#include "program.h"
#include "parser.h"
#include "symtab.h"
#include "lexer.h"
#include "acia.h"
#include <string.h>

/*============================================================================
 * Global program instance
 *============================================================================*/
program_t program;

/*============================================================================
 * Program functions
 *============================================================================*/

void prog_init(void) {
    /* Clear counters */
    program.num_lines = 0;
    program.current_line = 0;
    program.running = FALSE;
    program.stopped = FALSE;
    program.do_depth = 0;
    program.if_depth = 0;
    program.num_subprogs = 0;
    program.call_depth = 0;

    /* Clear label table */
    label_init();
}

bool_t prog_add_line(const char *line, uint16_t label) {
    if (program.num_lines >= MAX_PROG_LINES) {
        return FALSE;
    }

    prog_line_t *pl = &prog_lines[program.num_lines];
    pl->label = label;
    strncpy(pl->text, line, MAX_LINE_LEN);
    pl->text[MAX_LINE_LEN] = '\0';

    /* Register label if present */
    if (label > 0) {
        if (!label_add(label, program.num_lines, program.num_lines)) {
            return FALSE;  /* Duplicate label */
        }
    }

    program.num_lines++;
    return TRUE;
}

int8_t prog_find_label(uint16_t label) {
    label_t *lbl = label_lookup(label);
    if (lbl != NULL) {
        return (int8_t)lbl->line;
    }
    return -1;
}

const char *prog_get_line(void) {
    if (program.current_line < program.num_lines) {
        return prog_lines[program.current_line].text;
    }
    return NULL;
}

void prog_next_line(void) {
    if (program.current_line < program.num_lines) {
        program.current_line++;
    }
}

bool_t prog_goto(uint16_t label) {
    int8_t idx = prog_find_label(label);
    if (idx >= 0) {
        program.current_line = (uint8_t)idx;
        return TRUE;
    }
    return FALSE;
}

bool_t prog_push_do(uint16_t end_label, const char *var_name,
                     int16_t end_val, int16_t step_val) {
    if (program.do_depth >= MAX_DO_NEST) {
        return FALSE;
    }

    do_state_t *ds = &program.do_stack[program.do_depth];
    ds->end_label = end_label;
    ds->line_idx = program.current_line;
    strncpy(ds->var_name, var_name, MAX_IDENT_LEN);
    ds->var_name[MAX_IDENT_LEN] = '\0';
    ds->end_val = end_val;
    ds->step_val = step_val;

    program.do_depth++;
    return TRUE;
}

bool_t prog_pop_do(uint16_t label) {
    /* Find matching DO for this CONTINUE label */
    for (uint8_t i = program.do_depth; i > 0; i--) {
        do_state_t *ds = &program.do_stack[i - 1];
        if (ds->end_label == label) {
            /* Found matching DO - check loop condition */
            symbol_t *sym = sym_lookup(ds->var_name);
            if (sym == NULL || sym->type != TYPE_INTEGER) {
                return FALSE;
            }

            int16_t val = sym_get_int(sym, 0);
            val += ds->step_val;
            sym_set_int(sym, 0, val);

            /* Check if loop should continue */
            bool_t done;
            if (ds->step_val > 0) {
                done = (val > ds->end_val);
            } else {
                done = (val < ds->end_val);
            }

            if (!done) {
                /* Loop again - go back to line after DO */
                program.current_line = ds->line_idx + 1;
                /* Set flag to skip normal line increment */
                program.do_depth |= 0x80;
            } else {
                /* Loop done - pop this DO */
                program.do_depth = i - 1;
            }
            return TRUE;
        }
    }
    return FALSE;  /* No matching DO found */
}

/*============================================================================
 * IF block functions
 *============================================================================*/

bool_t prog_push_if(bool_t condition) {
    if (program.if_depth >= MAX_IF_NEST) {
        return FALSE;
    }

    if_state_t *is = &program.if_stack[program.if_depth];
    is->line_idx = program.current_line;
    is->condition = condition;
    is->in_else = FALSE;

    program.if_depth++;
    return TRUE;
}

bool_t prog_handle_else(void) {
    if (program.if_depth == 0) {
        return FALSE;  /* ELSE without IF */
    }

    if_state_t *is = &program.if_stack[program.if_depth - 1];
    if (is->in_else) {
        return FALSE;  /* Already in ELSE - duplicate ELSE */
    }

    is->in_else = TRUE;

    /* If the IF condition was true, we skip the ELSE block */
    if (is->condition) {
        prog_skip_to_endif();
        return FALSE;  /* Don't execute ELSE block */
    }

    /* IF condition was false, execute ELSE block */
    return TRUE;
}

bool_t prog_pop_if(void) {
    if (program.if_depth == 0) {
        return FALSE;  /* ENDIF without IF */
    }

    program.if_depth--;
    return TRUE;
}

/* Helper to check if a line starts with a specific keyword */
static token_type_t get_line_keyword(const char *line) {
    lexer_t lex;
    lexer_init(&lex, line, 0);

    /* Skip optional label */
    if (lexer_current(&lex)->type == TK_INT_LIT) {
        lexer_next(&lex);
    }

    return lexer_current(&lex)->type;
}

void prog_skip_to_else_or_endif(void) {
    uint8_t nesting = 1;  /* We're inside one IF */

    while (nesting > 0 && program.current_line < program.num_lines - 1) {
        program.current_line++;
        const char *line = prog_lines[program.current_line].text;

        token_type_t kw = get_line_keyword(line);

        if (kw == TK_IF) {
            /* Check if this is a block IF (has THEN) */
            lexer_t lex;
            lexer_init(&lex, line, 0);
            /* Skip label if present */
            if (lexer_current(&lex)->type == TK_INT_LIT) lexer_next(&lex);
            /* Skip IF */
            lexer_next(&lex);
            /* Skip condition - look for THEN at end */
            uint8_t paren_depth = 0;
            while (!lexer_at_eol(&lex)) {
                token_type_t t = lexer_current(&lex)->type;
                if (t == TK_LPAREN) paren_depth++;
                else if (t == TK_RPAREN) paren_depth--;
                else if (t == TK_THEN && paren_depth == 0) {
                    nesting++;
                    break;
                }
                lexer_next(&lex);
            }
        } else if (kw == TK_ELSE && nesting == 1) {
            /* Found our ELSE - stop here (will be processed by parser) */
            return;
        } else if (kw == TK_ENDIF) {
            nesting--;
            if (nesting == 0) {
                /* Found our ENDIF */
                return;
            }
        }
    }
}

void prog_skip_to_endif(void) {
    uint8_t nesting = 1;  /* We're inside one IF */

    while (nesting > 0 && program.current_line < program.num_lines - 1) {
        program.current_line++;
        const char *line = prog_lines[program.current_line].text;

        token_type_t kw = get_line_keyword(line);

        if (kw == TK_IF) {
            /* Check if this is a block IF (has THEN) */
            lexer_t lex;
            lexer_init(&lex, line, 0);
            /* Skip label if present */
            if (lexer_current(&lex)->type == TK_INT_LIT) lexer_next(&lex);
            /* Skip IF */
            lexer_next(&lex);
            /* Skip condition - look for THEN at end */
            uint8_t paren_depth = 0;
            while (!lexer_at_eol(&lex)) {
                token_type_t t = lexer_current(&lex)->type;
                if (t == TK_LPAREN) paren_depth++;
                else if (t == TK_RPAREN) paren_depth--;
                else if (t == TK_THEN && paren_depth == 0) {
                    nesting++;
                    break;
                }
                lexer_next(&lex);
            }
        } else if (kw == TK_ENDIF) {
            nesting--;
        }
    }
}

bool_t prog_at_end(void) {
    return program.current_line >= program.num_lines || program.stopped;
}

bool_t prog_run(void) {
    parser_t parser;

    program.current_line = 0;
    program.running = TRUE;
    program.stopped = FALSE;
    program.do_depth = 0;
    program.if_depth = 0;
    program.call_depth = 0;

    /* Reset variables but keep declarations */
    sym_init();

    while (!prog_at_end()) {
        const char *line = prog_get_line();
        if (line == NULL) break;

        parser_init(&parser, line, program.current_line + 1);

        if (!parse_statement(&parser)) {
            acia_puts("Error at line ");
            /* Print line number */
            uint8_t n = program.current_line + 1;
            char buf[4];
            uint8_t i = 0;
            do {
                buf[i++] = '0' + (n % 10);
                n /= 10;
            } while (n > 0);
            while (i > 0) acia_putc(buf[--i]);

            acia_puts(": ");
            acia_puts(parser_error_msg(&parser));
            acia_newline();
            program.running = FALSE;
            return FALSE;
        }

        /* Check for GOTO flag (bit 7 of do_depth) */
        if (program.do_depth & 0x80) {
            program.do_depth &= 0x7F;  /* Clear flag */
            /* Don't increment - GOTO already set target */
        } else {
            prog_next_line();
        }
    }

    program.running = FALSE;
    return TRUE;
}

/*============================================================================
 * Subprogram functions
 *============================================================================*/

subprog_t *prog_add_subprog(const char *name, uint8_t type, uint8_t return_type) {
    if (program.num_subprogs >= MAX_SUBPROGRAMS) {
        return NULL;
    }

    subprog_t *sp = &program.subprogs[program.num_subprogs];
    strncpy(sp->name, name, MAX_IDENT_LEN);
    sp->name[MAX_IDENT_LEN] = '\0';
    sp->type = type;
    sp->return_type = return_type;
    sp->start_line = program.current_line + 1;  /* Body starts on next line */
    sp->end_line = 0;  /* Will be set when END is found */
    sp->num_params = 0;

    program.num_subprogs++;
    return sp;
}

subprog_t *prog_find_subprog(const char *name) {
    for (uint8_t i = 0; i < program.num_subprogs; i++) {
        if (strcmp(program.subprogs[i].name, name) == 0) {
            return &program.subprogs[i];
        }
    }
    return NULL;
}

bool_t prog_push_call(uint8_t return_line, uint8_t subprog_idx) {
    if (program.call_depth >= MAX_CALL_DEPTH) {
        return FALSE;
    }

    call_frame_t *frame = &program.call_stack[program.call_depth];
    frame->return_line = return_line;
    frame->subprog_idx = subprog_idx;
    frame->saved_do_depth = program.do_depth & 0x7F;  /* Mask out GOTO flag */
    frame->saved_if_depth = program.if_depth;

    program.call_depth++;
    return TRUE;
}

bool_t prog_pop_call(void) {
    if (program.call_depth == 0) {
        return FALSE;
    }

    program.call_depth--;
    call_frame_t *frame = &program.call_stack[program.call_depth];

    /* Restore stacks to the state at call time */
    program.do_depth = frame->saved_do_depth;
    program.if_depth = frame->saved_if_depth;

    /* Return to caller */
    program.current_line = frame->return_line;
    program.do_depth |= 0x80;  /* Set GOTO flag to prevent increment */

    return TRUE;
}

bool_t prog_in_subprog(void) {
    return program.call_depth > 0;
}

void prog_skip_subprog(void) {
    /* Skip to matching END */
    while (program.current_line < program.num_lines - 1) {
        program.current_line++;
        const char *line = prog_lines[program.current_line].text;

        token_type_t kw = get_line_keyword(line);
        if (kw == TK_END) {
            /* Found END - stop here */
            return;
        }
    }
}
