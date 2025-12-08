# kz80_fortran

A Fortran 77 subset interpreter for the RetroShield Z80 platform, written in C and cross-compiled with SDCC.

## Hardware Requirements

**Teensy adapter required.** This interpreter requires approximately 8KB of RAM for program storage, variables, and runtime data. The Arduino Mega only provides ~4KB of usable RAM, which is insufficient. The Teensy 3.5/3.6/4.1 adapter provides 256KB of RAM.

See: [Teensy Adapter for RetroShield](https://www.tindie.com/products/8bitforce/teensy-adapter-for-retroshield/)

## Features

- Free-format source input (no column requirements)
- REAL numbers via BCD floating point (8 significant digits)
- Interactive REPL with program entry mode
- Block IF/THEN/ELSE/ENDIF statements
- DO loops with optional step
- 1D arrays
- SUBROUTINE and FUNCTION support
- Intrinsic functions: ABS, MOD, INT, REAL, SQRT

## Supported Statements

| Statement | Example |
|-----------|---------|
| PROGRAM/END | `PROGRAM HELLO` ... `END` |
| INTEGER/REAL | `INTEGER I, J` / `REAL X, Y` |
| DIMENSION | `DIMENSION ARR(10)` |
| Assignment | `X = 3.14` |
| IF (immediate) | `IF (X .GT. 0) WRITE(*,*) 'POS'` |
| IF/THEN/ELSE/ENDIF | `IF (X .GT. 0) THEN` ... `ENDIF` |
| DO/CONTINUE | `DO 10 I = 1, 10, 2` ... `10 CONTINUE` |
| GOTO | `GOTO 100` |
| WRITE | `WRITE(*,*) 'Hello', X` |
| READ | `READ(*,*) X, Y` |
| STOP | `STOP` |
| SUBROUTINE | `SUBROUTINE MYSUB(X, Y)` ... `END` |
| FUNCTION | `FUNCTION MYFUNC(X)` ... `END` |
| CALL | `CALL MYSUB(A, B)` |
| RETURN | `RETURN` |

## Supported Operators

- Arithmetic: `+`, `-`, `*`, `/`, `**` (power)
- Relational: `.EQ.`, `.NE.`, `.LT.`, `.GT.`, `.LE.`, `.GE.`
- Logical: `.AND.`, `.OR.`, `.NOT.`

## Building

Requires SDCC (Small Device C Compiler) for Z80.

```bash
make            # Build the interpreter
make clean      # Clean build artifacts
```

## Running

Requires the RetroShield Z80 emulator.

```bash
make run        # Run in TUI emulator
make run-simple # Run in passthrough mode
```

## Usage

The interpreter starts in immediate mode. You can:

1. **Execute statements directly:**
   ```
   > INTEGER X
   > X = 42
   > WRITE(*,*) X
   42
   ```

2. **Enter a program:**
   ```
   > PROGRAM HELLO
     INTEGER I
     DO 10 I = 1, 5
       WRITE(*,*) 'COUNT:', I
   10 CONTINUE
     END
   Program entered. Type RUN to execute.
   > RUN
   COUNT: 1
   COUNT: 2
   COUNT: 3
   COUNT: 4
   COUNT: 5
   ```

3. **Commands:**
   - `RUN` - Execute the stored program
   - `LIST` - Display the stored program
   - `NEW` - Clear the stored program

## Testing

The project includes both C unit tests and FORTRAN integration tests.

### Running All Tests

```bash
make test
```

### C Unit Tests

Located in `tests/c/`. Tests the core modules on the host system.

```bash
make test-c
```

Tests include:
- **test_bcd** - BCD floating point library (57 tests)
  - Conversion, parsing, arithmetic, comparison, printing, square root
- **test_lexer** - Tokenizer (43 tests)
  - Literals, keywords, operators, statement tokenization
- **test_symtab** - Symbol table (24 tests)
  - Symbol add/lookup, integer/real get/set, arrays, labels
- **test_parser** - Expression parser (40 tests)
  - Integer/real literals, arithmetic, relational/logical operators
  - Operator precedence, parentheses, variables, arrays, intrinsics
- **test_program** - Program control flow (29 tests)
  - Line management, DO loop stack, IF block stack
  - Subprogram management, call stack, program state

### FORTRAN Integration Tests

Located in `tests/fortran/`. These run actual FORTRAN programs on the interpreter.

```bash
make test-fortran
```

Test programs:
- **test_arithmetic.f** - Integer and real arithmetic operations
- **test_arrays.f** - Array declaration and element access
- **test_blockif.f** - Block IF/THEN/ELSE/ENDIF statements
- **test_comparison.f** - Comparison and logical operators
- **test_doloop.f** - DO loops with various step values
- **test_intrinsics.f** - ABS, MOD, INT, REAL functions
- **test_subprogs.f** - SUBROUTINE and FUNCTION calls

### Code Coverage

Generate a coverage report for the C unit tests using LLVM's coverage tools:

```bash
make coverage       # Text report to terminal
make coverage-html  # HTML report in tests/c/coverage_html/
```

Current coverage:
| Module | Line Coverage | Branch Coverage | Function Coverage |
|--------|---------------|-----------------|-------------------|
| bcd.c | 87.95% | 83.06% | 90.91% |
| lexer.c | 75.29% | 63.75% | 80.95% |

### Writing New Tests

**C Unit Tests:**
```c
#include "test_framework.h"

void test_example(void) {
    TEST("description");
    ASSERT_EQ(expected, actual);
    ASSERT_TRUE(condition);
    ASSERT_STR_EQ("expected", actual_str);
    TEST_PASS();
}
```

**FORTRAN Test Programs:**
- Create a file `tests/fortran/test_*.f`
- End with `WRITE(*,*) 'name TESTS PASSED'`
- The test runner looks for "TESTS PASSED" in output

## Memory Layout

```
$0000-$00FF   Vectors + init code
$0100-$4FFF   Interpreter code (~19KB)
$5000-$5BFF   C globals/DATA section (~3KB)
$6000-$6BFF   Program text storage (~3KB)
$7000-$77FF   Variable storage (~2KB)
$8000         Stack (grows down)
```

## Limitations

- Maximum 32 program lines
- Maximum 128 symbols (variables)
- Maximum 64 labels
- Maximum 8 nested DO loops
- Maximum 8 nested IF blocks
- Maximum 8 nested CALL depth
- Maximum 8 parameters per subprogram
- Maximum 8 subprograms (SUBROUTINE/FUNCTION)
- No COMMON blocks
- No multi-dimensional arrays (yet)
- No FORMAT statements (list-directed I/O only)

## Example Program

```fortran
PROGRAM AVERAGE
  INTEGER I, N
  REAL SUM, AVG, X

  WRITE(*,*) 'HOW MANY NUMBERS?'
  READ(*,*) N

  SUM = 0.0
  DO 10 I = 1, N
    WRITE(*,*) 'ENTER NUMBER', I
    READ(*,*) X
    SUM = SUM + X
10 CONTINUE

  AVG = SUM / REAL(N)
  WRITE(*,*) 'AVERAGE =', AVG
END
```

## License

Part of the RetroShield Arduino project.
