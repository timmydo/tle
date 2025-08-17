#!/bin/sh
# Test script for TLE (Timmy's Lisp Environment)

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