#!/bin/sh
# Test script for TLE (Timmy's Lisp Environment)

export CL_SOURCE_REGISTRY="$(pwd)/vendor//:"

echo "Starting TLE tests..."

# Start TLE in background
echo "Starting TLE server..."
./start.sh &
TLE_PID=$!

# Wait for server to start
echo "Waiting for server to start..."
sleep 5

# Run all tests in tests/ directory
TESTS_PASSED=0
TESTS_FAILED=0

# Run shell script tests
for test_file in tests/test_*.sh; do
    if [ -f "$test_file" ]; then
        echo "Running $test_file..."
        if "$test_file"; then
            TESTS_PASSED=$((TESTS_PASSED + 1))
        else
            TESTS_FAILED=$((TESTS_FAILED + 1))
        fi
        echo ""
    fi
done

# Run Lisp test files
for test_file in tests/test*.lisp; do
    if [ -f "$test_file" ]; then
        echo "Running Lisp tests from $test_file..."
        # Extract the test function name from the file (assumes run-all-*-tests pattern)
        test_function=$(grep -o "defun run-all-[^(]*" "$test_file" | head -1 | sed 's/defun //')
        if [ -n "$test_function" ]; then
            if sbcl --non-interactive \
                    --eval "(asdf:load-system :tle)" \
                    --eval "(load \"$test_file\")" \
                    --eval "(if (tle::$test_function) (sb-ext:exit :code 0) (sb-ext:exit :code 1))" 2>/dev/null; then
                TESTS_PASSED=$((TESTS_PASSED + 1))
                echo "✓ Lisp tests in $test_file passed"
            else
                TESTS_FAILED=$((TESTS_FAILED + 1))
                echo "✗ Lisp tests in $test_file failed"
            fi
        else
            echo "⚠ No test function found in $test_file (expected run-all-*-tests pattern)"
        fi
        echo ""
    fi
done

# Clean up - kill TLE server
echo "Stopping TLE server..."
kill $TLE_PID 2>/dev/null
wait $TLE_PID 2>/dev/null

echo "Tests completed."
echo "Passed: $TESTS_PASSED, Failed: $TESTS_FAILED"

if [ $TESTS_FAILED -gt 0 ]; then
    exit 1
else
    exit 0
fi