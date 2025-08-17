#!/bin/sh
# Test: HTTP GET request with app parameter

echo "Test: Testing HTTP GET request with app parameter"
RESPONSE=$(curl -s -w "%{http_code}" "http://localhost:8080/?app=testapp")
HTTP_CODE=$(echo "$RESPONSE" | tail -c 4)
BODY=$(echo "$RESPONSE" | head -c -4)

if [ "$HTTP_CODE" = "200" ]; then
    echo "✓ HTTP GET test with app parameter passed (status: $HTTP_CODE)"
    if echo "$BODY" | grep -q "TLE"; then
        echo "✓ Response body contains expected content"
        exit 0
    else
        echo "✗ Response body does not contain expected content"
        echo "Got: $BODY"
        exit 1
    fi
else
    echo "✗ HTTP GET test with app parameter failed (status: $HTTP_CODE)"
    echo "Response: $RESPONSE"
    exit 1
fi
