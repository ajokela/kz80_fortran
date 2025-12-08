# Makefile for kz80_fortran C unit tests
# These tests run on the host system, not the Z80

CC = cc
CFLAGS = -Wall -Wextra -g -I../../include

# Coverage flags (use with 'make coverage')
COV_FLAGS = --coverage -fprofile-instr-generate -fcoverage-mapping

# Test executables
TESTS = test_bcd test_lexer test_symtab test_parser test_program

# Coverage data files
COV_DATA = *.profraw *.profdata *.gcda *.gcno

.PHONY: all clean test coverage coverage-report coverage-html

all: $(TESTS)

test: all
	@echo "Running BCD tests..."
	@./test_bcd
	@echo ""
	@echo "Running Lexer tests..."
	@./test_lexer
	@echo ""
	@echo "Running Symbol Table tests..."
	@./test_symtab
	@echo ""
	@echo "Running Parser tests..."
	@./test_parser
	@echo ""
	@echo "Running Program tests..."
	@./test_program

test_bcd: test_bcd.c test_framework.h mock_acia.h ../../src/bcd.c ../../include/bcd.h ../../include/types.h
	$(CC) $(CFLAGS) -o $@ test_bcd.c

test_lexer: test_lexer.c test_framework.h mock_acia.h ../../src/lexer.c ../../include/lexer.h ../../include/types.h
	$(CC) $(CFLAGS) -o $@ test_lexer.c

test_symtab: test_symtab.c test_framework.h mock_acia.h ../../src/symtab.c ../../include/symtab.h ../../include/types.h
	$(CC) $(CFLAGS) -o $@ test_symtab.c

test_parser: test_parser.c test_framework.h mock_acia.h ../../src/parser.c ../../include/parser.h ../../include/types.h
	$(CC) $(CFLAGS) -o $@ test_parser.c

test_program: test_program.c test_framework.h mock_acia.h ../../src/program.c ../../include/program.h ../../include/types.h
	$(CC) $(CFLAGS) -o $@ test_program.c

# Build with coverage instrumentation
test_bcd_cov: test_bcd.c test_framework.h mock_acia.h ../../src/bcd.c ../../include/bcd.h ../../include/types.h
	$(CC) $(CFLAGS) $(COV_FLAGS) -o $@ test_bcd.c

test_lexer_cov: test_lexer.c test_framework.h mock_acia.h ../../src/lexer.c ../../include/lexer.h ../../include/types.h
	$(CC) $(CFLAGS) $(COV_FLAGS) -o $@ test_lexer.c

test_symtab_cov: test_symtab.c test_framework.h mock_acia.h ../../src/symtab.c ../../include/symtab.h ../../include/types.h
	$(CC) $(CFLAGS) $(COV_FLAGS) -o $@ test_symtab.c

# Run tests with coverage collection
coverage: test_bcd_cov test_lexer_cov test_symtab_cov
	@echo "Running BCD tests with coverage..."
	@LLVM_PROFILE_FILE="test_bcd.profraw" ./test_bcd_cov
	@echo ""
	@echo "Running Lexer tests with coverage..."
	@LLVM_PROFILE_FILE="test_lexer.profraw" ./test_lexer_cov
	@echo ""
	@echo "Running Symbol Table tests with coverage..."
	@LLVM_PROFILE_FILE="test_symtab.profraw" ./test_symtab_cov
	@echo ""
	@echo "Merging coverage data..."
	@xcrun llvm-profdata merge -sparse test_bcd.profraw test_lexer.profraw test_symtab.profraw -o tests.profdata

# Generate text coverage report
coverage-report: coverage
	@echo ""
	@echo "========================================"
	@echo "Coverage Report for bcd.c"
	@echo "========================================"
	@xcrun llvm-cov report ./test_bcd_cov -instr-profile=tests.profdata ../../src/bcd.c
	@echo ""
	@echo "========================================"
	@echo "Coverage Report for lexer.c"
	@echo "========================================"
	@xcrun llvm-cov report ./test_lexer_cov -instr-profile=tests.profdata ../../src/lexer.c
	@echo ""
	@echo "========================================"
	@echo "Coverage Report for symtab.c"
	@echo "========================================"
	@xcrun llvm-cov report ./test_symtab_cov -instr-profile=tests.profdata ../../src/symtab.c

# Generate HTML coverage report
coverage-html: coverage
	@mkdir -p coverage_html
	@xcrun llvm-cov show ./test_bcd_cov ./test_lexer_cov ./test_symtab_cov \
		-instr-profile=tests.profdata \
		-format=html \
		-output-dir=coverage_html \
		../../src/bcd.c ../../src/lexer.c ../../src/symtab.c
	@echo "HTML coverage report generated in coverage_html/"
	@echo "Open coverage_html/index.html in a browser to view"

clean:
	rm -f $(TESTS) test_bcd_cov test_lexer_cov test_symtab_cov test_parser_cov test_program_cov
	rm -f $(COV_DATA)
	rm -rf coverage_html
