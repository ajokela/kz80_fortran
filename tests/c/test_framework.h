/*
 * test_framework.h - Simple unit test framework for kz80_fortran
 */

#ifndef TEST_FRAMEWORK_H
#define TEST_FRAMEWORK_H

#include <stdio.h>
#include <string.h>

/* Test counters */
static int tests_run = 0;
static int tests_passed = 0;
static int tests_failed = 0;

/* Current test name */
static const char *current_test = NULL;

/* Start a test */
#define TEST(name) \
    do { \
        current_test = name; \
        tests_run++; \
    } while(0)

/* Assert macros */
#define ASSERT_TRUE(cond) \
    do { \
        if (!(cond)) { \
            printf("FAIL: %s - %s (line %d)\n", current_test, #cond, __LINE__); \
            tests_failed++; \
            return; \
        } \
    } while(0)

#define ASSERT_FALSE(cond) ASSERT_TRUE(!(cond))

#define ASSERT_EQ(expected, actual) \
    do { \
        if ((expected) != (actual)) { \
            printf("FAIL: %s - expected %d, got %d (line %d)\n", \
                   current_test, (int)(expected), (int)(actual), __LINE__); \
            tests_failed++; \
            return; \
        } \
    } while(0)

#define ASSERT_STR_EQ(expected, actual) \
    do { \
        if (strcmp((expected), (actual)) != 0) { \
            printf("FAIL: %s - expected \"%s\", got \"%s\" (line %d)\n", \
                   current_test, (expected), (actual), __LINE__); \
            tests_failed++; \
            return; \
        } \
    } while(0)

/* Mark test as passed (call at end of successful test) */
#define TEST_PASS() \
    do { \
        tests_passed++; \
        printf("PASS: %s\n", current_test); \
    } while(0)

/* Print test summary */
#define TEST_SUMMARY() \
    do { \
        printf("\n========================================\n"); \
        printf("Tests run: %d, Passed: %d, Failed: %d\n", \
               tests_run, tests_passed, tests_failed); \
        printf("========================================\n"); \
    } while(0)

/* Return exit code based on results */
#define TEST_EXIT_CODE() (tests_failed > 0 ? 1 : 0)

#endif /* TEST_FRAMEWORK_H */
