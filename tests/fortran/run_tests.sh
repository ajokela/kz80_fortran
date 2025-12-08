#!/bin/bash
#
# Run FORTRAN77 test programs on the kz80_fortran interpreter
#

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR/../.."
EMULATOR="$PROJECT_DIR/../emulator/rust/target/release/retroshield"
BINARY="$PROJECT_DIR/bin/fortran77.bin"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Check if binary exists
if [ ! -f "$BINARY" ]; then
    echo -e "${RED}Error: $BINARY not found. Run 'make' first.${NC}"
    exit 1
fi

# Check if emulator exists
if [ ! -f "$EMULATOR" ]; then
    echo -e "${RED}Error: $EMULATOR not found.${NC}"
    exit 1
fi

# Function to run a single test
run_test() {
    local test_file="$1"
    local test_name=$(basename "$test_file" .f)

    echo -n "Testing $test_name... "
    TESTS_RUN=$((TESTS_RUN + 1))

    # Read the FORTRAN file, removing comment lines (C in column 1)
    # and convert to interpreter input format
    local input=""
    while IFS= read -r line; do
        # Skip lines starting with C (comments)
        if [[ ! "$line" =~ ^[Cc] ]]; then
            # Trim leading whitespace (FORTRAN uses columns 7-72)
            line="${line#"${line%%[![:space:]]*}"}"
            if [ -n "$line" ]; then
                input+="$line"$'\n'
            fi
        fi
    done < "$test_file"
    input+="RUN"$'\n'

    # Run the test with timeout
    local output
    output=$(printf "%s" "$input" | timeout 5 "$EMULATOR" "$BINARY" 2>&1)
    local exit_code=$?

    # Check for success marker
    if echo "$output" | grep -q "TESTS PASSED"; then
        echo -e "${GREEN}PASSED${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        echo -e "${RED}FAILED${NC}"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        echo -e "${YELLOW}Output:${NC}"
        echo "$output" | head -50
        echo ""
        return 1
    fi
}

echo "========================================"
echo "FORTRAN77 Test Suite"
echo "========================================"
echo ""

# Run all test files
for test_file in "$SCRIPT_DIR"/test_*.f; do
    if [ -f "$test_file" ]; then
        run_test "$test_file"
    fi
done

echo ""
echo "========================================"
echo "Results: $TESTS_PASSED/$TESTS_RUN passed"
if [ $TESTS_FAILED -gt 0 ]; then
    echo -e "${RED}$TESTS_FAILED test(s) failed${NC}"
    exit 1
else
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
fi
