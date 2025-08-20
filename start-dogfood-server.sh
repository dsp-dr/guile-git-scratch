#!/bin/sh

# Start Guile Git Server in tmux for dogfooding

SESSION_NAME="guile-git-server"
SERVER_PORT=9418
REPOS_DIR="./dogfood-repos"

echo "=== Guile Git Dogfood Server Setup ==="
echo

# Check if tmux is installed
if ! command -v tmux > /dev/null; then
    echo "Error: tmux is not installed"
    echo "Install with: pkg install tmux"
    exit 1
fi

# Kill existing session if it exists
tmux kill-session -t $SESSION_NAME 2>/dev/null

# Create repos directory
mkdir -p $REPOS_DIR

# Create new tmux session
echo "Starting tmux session: $SESSION_NAME"
tmux new-session -d -s $SESSION_NAME

# Send commands to start server
tmux send-keys -t $SESSION_NAME "cd $(pwd)" C-m
tmux send-keys -t $SESSION_NAME "echo '=== Guile Git Server for Dogfooding ==='" C-m
tmux send-keys -t $SESSION_NAME "echo 'Port: $SERVER_PORT'" C-m
tmux send-keys -t $SESSION_NAME "echo 'Repos: $REPOS_DIR'" C-m
tmux send-keys -t $SESSION_NAME "echo ''" C-m
tmux send-keys -t $SESSION_NAME "echo 'Choose your server:'" C-m
tmux send-keys -t $SESSION_NAME "echo '1. ./minimal-server.scm (one connection then exit - good for testing)'" C-m
tmux send-keys -t $SESSION_NAME "echo '2. ./minimal-server-daemon.scm (stays running - good for dogfooding)'" C-m
tmux send-keys -t $SESSION_NAME "echo ''" C-m
tmux send-keys -t $SESSION_NAME "echo 'Starting daemon version for continuous dogfooding...'" C-m
tmux send-keys -t $SESSION_NAME "./minimal-server-daemon.scm" C-m

echo "✓ Server started in tmux session: $SESSION_NAME"
echo
echo "Commands:"
echo "  View server:    tmux attach -t $SESSION_NAME"
echo "  Detach:         Ctrl-B then D"
echo "  Stop server:    tmux kill-session -t $SESSION_NAME"
echo
echo "Git remote setup:"
echo "  git remote add dogfood git://localhost:$SERVER_PORT/[repo-name].git"
echo "  git push dogfood main"
echo
echo "The server is now running and ready for dogfooding!"