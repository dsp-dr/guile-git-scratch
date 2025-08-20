#!/bin/sh

# Test script for dogfooding the Guile Git server

echo "=== Guile Git Server Dogfood Test ==="
echo

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Configuration
SERVER_PORT=9419
REPOS_DIR="./test-repos"
TEST_DIR="/tmp/guile-git-test-$$"

# Cleanup function
cleanup() {
    echo "Cleaning up..."
    # Kill server if running
    if [ -n "$SERVER_PID" ]; then
        kill $SERVER_PID 2>/dev/null
    fi
    # Remove test directory
    rm -rf "$TEST_DIR"
}

trap cleanup EXIT

echo "1. Starting Guile Git server on port $SERVER_PORT..."
gmake test-server &
SERVER_PID=$!
sleep 2

echo "2. Creating test directory: $TEST_DIR"
mkdir -p "$TEST_DIR"
cd "$TEST_DIR"

echo "3. Creating a test repository..."
git init test-client
cd test-client

echo "4. Adding initial content..."
echo "# Test Repository" > README.md
echo "Testing Guile Git server" >> README.md
git add README.md
git commit -m "Initial commit"

echo "5. Adding Guile Git server as remote..."
git remote add guile-server git://localhost:$SERVER_PORT/test.git

echo "6. Attempting to push to Guile Git server..."
if git push guile-server main 2>&1; then
    echo "${GREEN}✓ Push successful!${NC}"
else
    echo "${RED}✗ Push failed${NC}"
    echo "Note: The minimal server may not fully support pack format yet."
    echo "This is expected in the initial implementation."
fi

echo
echo "7. Server log should show connection attempts above."
echo
echo "Test complete. Press Ctrl+C to stop the server."

# Keep server running for manual testing
wait $SERVER_PID