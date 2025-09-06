#!/bin/sh
# Test script for TLE (Timmy's Lisp Environment)

export CL_SOURCE_REGISTRY="$(pwd)//:"

# Parse command line arguments
VERBOSE=0
if [ "$1" = "-v" ] || [ "$1" = "--verbose" ]; then
    VERBOSE=1
    shift
elif [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Run TLE (Timmy's Lisp Environment) tests"
    echo ""
    echo "OPTIONS:"
    echo "  -v, --verbose    Show all test output (verbose mode)"
    echo "  -h, --help       Show this help message"
    echo ""
    echo "Default mode is quiet - only failures are shown."
    echo "Use --verbose to see all test output including successes."
    exit 0
fi

if [ $VERBOSE -eq 1 ]; then
    echo "Starting TLE tests (verbose mode)..."
else
    echo "Starting TLE tests (quiet mode - only failures shown)..."
fi

# Start TLE in background
if [ $VERBOSE -eq 1 ]; then
    echo "Starting TLE server..."
fi
./start.sh &
TLE_PID=$!

# Wait for server to start
if [ $VERBOSE -eq 1 ]; then
    echo "Waiting for server to start..."
fi
sleep 5

# Run all tests in tests/ directory
TESTS_PASSED=0
TESTS_FAILED=0

# Run shell script tests
for test_file in tests/test_*.sh; do
    if [ -f "$test_file" ]; then
        if [ $VERBOSE -eq 1 ]; then
            echo "Running $test_file..."
            if "$test_file"; then
                TESTS_PASSED=$((TESTS_PASSED + 1))
            else
                TESTS_FAILED=$((TESTS_FAILED + 1))
            fi
            echo ""
        else
            # Quiet mode - capture output and only show on failure
            if output=$("$test_file" 2>&1); then
                TESTS_PASSED=$((TESTS_PASSED + 1))
            else
                TESTS_FAILED=$((TESTS_FAILED + 1))
                echo "FAILED: $test_file"
                echo "$output"
                echo ""
            fi
        fi
    fi
done

# Run Lisp test files
for test_file in tests/test*.lisp; do
    if [ -f "$test_file" ]; then
        # Extract the test function name from the file (assumes run-all-*-tests pattern)
        test_function=$(grep -o "defun run-all-[^(]*" "$test_file" | head -1 | sed 's/defun //')
        if [ -n "$test_function" ]; then
            if [ $VERBOSE -eq 1 ]; then
                echo "Running Lisp tests from $test_file..."
                if sbcl --noinform --no-userinit --no-sysinit --non-interactive \
                        --eval "(require \"asdf\")" \
                        --eval "(sb-int:set-floating-point-modes :traps nil)" \
                        --eval "(asdf:load-system :tle)" \
                        --eval "(load \"$test_file\")" \
                        --eval "(if (tle::$test_function) (sb-ext:exit :code 0) (sb-ext:exit :code 1))"; then
                    TESTS_PASSED=$((TESTS_PASSED + 1))
                    echo "✓ Lisp tests in $test_file passed"
                else
                    TESTS_FAILED=$((TESTS_FAILED + 1))
                    echo "✗ Lisp tests in $test_file failed"
                fi
                echo ""
            else
                # Quiet mode - capture output and only show on failure
                if output=$(sbcl --noinform --no-userinit --no-sysinit --non-interactive \
                        --eval "(require \"asdf\")" \
                        --eval "(sb-int:set-floating-point-modes :traps nil)" \
                        --eval "(asdf:load-system :tle)" \
                        --eval "(load \"$test_file\")" \
                        --eval "(if (tle::$test_function) (sb-ext:exit :code 0) (sb-ext:exit :code 1))" 2>&1); then
                    TESTS_PASSED=$((TESTS_PASSED + 1))
                else
                    TESTS_FAILED=$((TESTS_FAILED + 1))
                    echo "FAILED: $test_file"
                    echo "$output"
                    echo ""
                fi
            fi
        else
            echo "⚠ No test function found in $test_file (expected run-all-*-tests pattern)"
        fi
    fi
done

# Clean up - kill TLE server
if [ $VERBOSE -eq 1 ]; then
    echo "Stopping TLE server..."
fi
kill $TLE_PID 2>/dev/null
wait $TLE_PID 2>/dev/null

echo "Tests completed."
echo "Passed: $TESTS_PASSED, Failed: $TESTS_FAILED"

if [ $TESTS_FAILED -gt 0 ]; then
    exit 1
else
    exit 0
fi
