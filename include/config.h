/*
 * config.h - Feature configuration for kz80_fortran
 *
 * Enable/disable features by commenting out or defining these macros.
 * Disabling features reduces code size to fit in memory constraints.
 *
 * Memory layout:
 *   Code must fit in 0x0100-0x4FFF (~20KB)
 *   Current baseline (all disabled): ~18KB
 *   Each feature adds approximately the indicated size.
 */

#ifndef CONFIG_H
#define CONFIG_H

/*============================================================================
 * Core features (always enabled)
 *============================================================================*/
/* These cannot be disabled:
 * - Basic expressions (arithmetic, comparisons, logical)
 * - INTEGER and REAL types
 * - Variables and 1D arrays
 * - IF/THEN/ELSE/ENDIF
 * - DO loops
 * - GOTO
 * - WRITE/READ (list-directed I/O)
 * - STOP/END
 */

/*============================================================================
 * Optional features - uncomment to enable
 *============================================================================*/

/* Multi-dimensional arrays (2D, 3D)
 * Adds: ~200 bytes
 * Enables: INTEGER A(10,10), REAL B(5,5,5)
 */
/* #define FEATURE_MULTIDIM_ARRAYS */

/* SQRT intrinsic function
 * Adds: ~400 bytes (includes Newton-Raphson implementation)
 * Enables: X = SQRT(Y)
 */
/* #define FEATURE_SQRT */

/* Additional intrinsic functions: ABS, MOD, INT
 * Adds: ~300 bytes
 * Enables: ABS(X), MOD(A,B), INT(X)
 * Note: REAL(X) conversion is always available
 */
/* #define FEATURE_INTRINSICS */

/* DATA statement for variable initialization
 * Adds: ~800 bytes
 * Enables: DATA X, Y, Z / 1.0, 2.0, 3.0 /
 */
/* #define FEATURE_DATA */

/* COMMON blocks for shared variables
 * Adds: ~600 bytes
 * Enables: COMMON /BLOCK/ X, Y, Z
 */
/* #define FEATURE_COMMON */

/* SUBROUTINE/FUNCTION/CALL/RETURN
 * Adds: ~2500 bytes
 * Enables: SUBROUTINE FOO(X), FUNCTION BAR(X), CALL FOO(1), RETURN
 */
/* #define FEATURE_SUBPROGRAMS */

/* FORMAT statements (parsing only, list-directed output used)
 * Adds: ~100 bytes
 * Enables: 100 FORMAT(I5, F8.2) - parsed but output is list-directed
 */
/* #define FEATURE_FORMAT */

/* Full BCD printing (scientific notation for all values)
 * If disabled, uses simplified scientific notation only
 * Adds: ~500 bytes
 * Enables: Prettier output like "123.45" instead of "1.23450E+02"
 */
/* #define FEATURE_PRETTY_PRINT */

/*============================================================================
 * Feature size summary (measured with SDCC)
 *============================================================================
 * Baseline (core only):           ~21,078 bytes
 * + FEATURE_MULTIDIM_ARRAYS:        +298 bytes
 * + FEATURE_SQRT:                   +565 bytes
 * + FEATURE_INTRINSICS:             +389 bytes
 * + FEATURE_FORMAT:                  +91 bytes
 * + FEATURE_DATA:                   TBD (currently stub)
 * + FEATURE_COMMON:                 TBD (currently stub)
 * + FEATURE_SUBPROGRAMS:            TBD (currently stub)
 * + FEATURE_PRETTY_PRINT:           TBD (not yet implemented)
 *
 * The 20KB code limit (0x5000 - 0x0100) = 20,224 bytes
 *
 * Baseline exceeds limit by ~850 bytes. Further core optimization needed
 * or increase code space in memory map if hardware allows.
 *
 * With all tested features:       ~22,496 bytes
 *============================================================================*/

#endif /* CONFIG_H */
