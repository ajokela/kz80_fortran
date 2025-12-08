/*
 * program.h - Program storage and execution for kz80_fortran
 */

#ifndef PROGRAM_H
#define PROGRAM_H

#include "types.h"

/*============================================================================
 * Program line entry
 *============================================================================*/
typedef struct {
    uint16_t label;             /* Statement label (0 if none) */
    char     text[MAX_LINE_LEN + 1];  /* Line text */
} prog_line_t;

/*============================================================================
 * DO loop state
 *============================================================================*/
typedef struct {
    uint16_t end_label;         /* Label at end of DO */
    uint8_t  line_idx;          /* Index of DO statement */
    char     var_name[MAX_IDENT_LEN + 1];  /* Loop variable */
    int16_t  end_val;           /* End value */
    int16_t  step_val;          /* Step value */
} do_state_t;

/*============================================================================
 * IF block state
 *============================================================================*/
typedef struct {
    uint8_t  line_idx;          /* Index of IF statement */
    bool_t   condition;         /* True if condition was true */
    bool_t   in_else;           /* True if we're in the ELSE branch */
} if_state_t;

#define MAX_IF_NEST     8       /* Maximum IF nesting */

/*============================================================================
 * Subprogram (SUBROUTINE/FUNCTION) definition
 *============================================================================*/
#define SUBPROG_SUBROUTINE  0
#define SUBPROG_FUNCTION    1

typedef struct {
    char     name[MAX_IDENT_LEN + 1];   /* Subprogram name */
    uint8_t  type;                       /* SUBPROG_SUBROUTINE or SUBPROG_FUNCTION */
    uint8_t  return_type;                /* Return type for FUNCTION (TYPE_INTEGER/REAL) */
    uint8_t  start_line;                 /* First line of body (after SUBROUTINE/FUNCTION) */
    uint8_t  end_line;                   /* Line with END */
    uint8_t  num_params;                 /* Number of parameters */
    char     params[MAX_PARAMS][MAX_IDENT_LEN + 1]; /* Parameter names */
    uint8_t  param_types[MAX_PARAMS];    /* Parameter types */
} subprog_t;

/*============================================================================
 * Call stack frame
 *============================================================================*/
typedef struct {
    uint8_t  return_line;               /* Line to return to after call */
    uint8_t  subprog_idx;               /* Index of called subprogram */
    uint8_t  saved_do_depth;            /* Saved DO stack depth */
    uint8_t  saved_if_depth;            /* Saved IF stack depth */
} call_frame_t;

/*============================================================================
 * Program state
 *============================================================================*/
typedef struct {
    uint8_t     num_lines;              /* Number of lines */
    uint8_t     current_line;           /* Current execution line */
    bool_t      running;                /* True if program is running */
    bool_t      stopped;                /* True if STOP encountered */

    /* DO loop stack */
    do_state_t  do_stack[MAX_DO_NEST];
    uint8_t     do_depth;

    /* IF block stack */
    if_state_t  if_stack[MAX_IF_NEST];
    uint8_t     if_depth;

    /* Subprogram table */
    subprog_t   subprogs[MAX_SUBPROGRAMS];
    uint8_t     num_subprogs;

    /* Call stack */
    call_frame_t call_stack[MAX_CALL_DEPTH];
    uint8_t      call_depth;
} program_t;

/* Program lines stored at MEM_PROGRAM */
#define prog_lines ((prog_line_t *)MEM_PROGRAM)

/*============================================================================
 * Global program instance
 *============================================================================*/
extern program_t program;

/*============================================================================
 * Program functions
 *============================================================================*/

/* Initialize/clear program */
void prog_init(void);

/* Add a line to the program */
bool_t prog_add_line(const char *line, uint16_t label);

/* Run the program */
bool_t prog_run(void);

/* Find line with label */
int8_t prog_find_label(uint16_t label);

/* Get current line for execution */
const char *prog_get_line(void);

/* Advance to next line */
void prog_next_line(void);

/* Jump to label */
bool_t prog_goto(uint16_t label);

/* Push DO state */
bool_t prog_push_do(uint16_t end_label, const char *var_name,
                     int16_t end_val, int16_t step_val);

/* Pop DO state (at CONTINUE) */
bool_t prog_pop_do(uint16_t label);

/* Push IF state (at IF...THEN) */
bool_t prog_push_if(bool_t condition);

/* Handle ELSE - returns TRUE if we should execute the ELSE branch */
bool_t prog_handle_else(void);

/* Pop IF state (at ENDIF) */
bool_t prog_pop_if(void);

/* Skip to matching ELSE or ENDIF (when IF condition is false) */
void prog_skip_to_else_or_endif(void);

/* Skip to matching ENDIF (when in true IF branch and hit ELSE) */
void prog_skip_to_endif(void);

/* Check if at end of program */
bool_t prog_at_end(void);

/*============================================================================
 * Subprogram functions
 *============================================================================*/

/* Add a subprogram definition */
subprog_t *prog_add_subprog(const char *name, uint8_t type, uint8_t return_type);

/* Look up subprogram by name */
subprog_t *prog_find_subprog(const char *name);

/* Push a call frame (returns FALSE if stack full) */
bool_t prog_push_call(uint8_t return_line, uint8_t subprog_idx);

/* Pop call frame and return to caller (returns FALSE if stack empty) */
bool_t prog_pop_call(void);

/* Check if we're inside a subprogram */
bool_t prog_in_subprog(void);

/* Skip subprogram body during main execution */
void prog_skip_subprog(void);

#endif /* PROGRAM_H */
