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

# Test 1: Basic HTTP GET request
echo "Test 1: Testing basic HTTP GET request to /"
RESPONSE=$(curl -s -w "%{http_code}" http://localhost:8080/)
HTTP_CODE=$(echo "$RESPONSE" | tail -c 4)
BODY=$(echo "$RESPONSE" | head -c -4)

if [ "$HTTP_CODE" = "200" ]; then
    echo "✓ HTTP GET test passed (status: $HTTP_CODE)"
    if echo "$BODY" | grep -q "Hello World"; then
        echo "✓ Response body contains expected content"
    else
        echo "✗ Response body does not contain expected content"
        echo "Got: $BODY"
    fi
else
    echo "✗ HTTP GET test failed (status: $HTTP_CODE)"
    echo "Response: $RESPONSE"
fi

# Clean up - kill TLE server
echo "Stopping TLE server..."
kill $TLE_PID 2>/dev/null
wait $TLE_PID 2>/dev/null

echo "Tests completed."