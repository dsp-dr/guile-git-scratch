#!/bin/sh
# 🐕 Woof AI Summary - Simple Shell Plugin for Ollama Integration
#
# This plugin generates AI-powered dog-themed summaries of Git pushes
# Usage: ./woof-ai-summary.sh [push-file-path]

OLLAMA_URL="http://localhost:11434/api/generate"
MODEL="qwen2.5-coder:7b"
WOOFS_DIR="data/woofs"

# Ensure woofs directory exists
mkdir -p "$WOOFS_DIR"

# Extract repository name from filename
extract_repo_name() {
    local filename="$1"
    basename "$filename" | sed 's/^[0-9]*-[0-9]*-//' | sed 's/\.txt$//'
}

# Generate AI summary using Ollama
generate_ai_summary() {
    local repo_name="$1"
    local push_data="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    # Create prompt
    local prompt="A developer just pushed to repository '$repo_name'. Here's the Git protocol data: $push_data. As a friendly dog analyst, explain what this developer accomplished in a fun, dog-themed way with plenty of dog puns!"
    
    # System prompt for the AI
    local system_prompt="You are a friendly dog who works as a Git commit analyst. Always respond with dog-themed humor and puns. Keep responses to 2-3 sentences. Use dog emojis and barking sounds. Focus on what the developer actually did, but make it fun!"
    
    # Call Ollama API
    local json_payload=$(cat <<EOF
{
    "model": "$MODEL",
    "prompt": "$prompt",
    "system": "$system_prompt", 
    "stream": false
}
EOF
    )
    
    local response=$(echo "$json_payload" | curl -s -X POST "$OLLAMA_URL" -H "Content-Type: application/json" -d @-)
    
    # Extract response text (simple grep/sed approach)
    local ai_text=$(echo "$response" | grep -o '"response":"[^"]*"' | sed 's/"response":"//; s/"$//' | sed 's/\\n/ /g; s/\\//g')
    
    if [ -z "$ai_text" ]; then
        ai_text="🐕 WOOF! AI dog is taking a nap, no summary available!"
    fi
    
    echo "🐕 AI WOOF ANALYSIS - $timestamp"
    echo ""
    echo "$ai_text"
}

# Save analysis to file
save_woof_analysis() {
    local repo_name="$1" 
    local push_data="$2"
    local summary="$3"
    local timestamp=$(date '+%Y%m%d-%H%M%S')
    local filename="$WOOFS_DIR/$timestamp-$repo_name-woof.txt"
    
    cat > "$filename" <<EOF
=== 🐕 OLLAMA WOOF ANALYSIS ===
Repository: $repo_name
Timestamp: $(date '+%Y-%m-%d %H:%M:%S')
Model: $MODEL

$summary

=== Original Push Data ===
$push_data
EOF
    
    echo "🐕 WOOF! Saved AI analysis to: $filename"
}

# Analyze a specific push file
analyze_push_file() {
    local push_file="$1"
    
    if [ ! -f "$push_file" ]; then
        echo "🐕 BARK! Push file not found: $push_file"
        return 1
    fi
    
    local repo_name=$(extract_repo_name "$push_file")
    local push_data=$(cat "$push_file")
    local summary=$(generate_ai_summary "$repo_name" "$push_data")
    
    echo "$summary"
    echo ""
    save_woof_analysis "$repo_name" "$push_data" "$summary"
}

# Analyze the latest push
analyze_latest_push() {
    local pushes_dir="data/pushes"
    
    if [ ! -d "$pushes_dir" ]; then
        echo "🐕 No pushes directory found!"
        return 1
    fi
    
    local latest_push=$(ls -t "$pushes_dir"/*.txt 2>/dev/null | head -1)
    
    if [ -z "$latest_push" ]; then
        echo "🐕 No push files found to analyze"
        return 1
    fi
    
    echo "🐕 WOOF! Analyzing latest push: $(basename "$latest_push")"
    echo ""
    analyze_push_file "$latest_push"
}

# Test with sample data
test_woof_analysis() {
    echo "🐕 TESTING WOOF AI ANALYSIS..."
    echo ""
    
    local test_data="git-receive-pack /test-doghouse.git host=localhost:9418"
    local summary=$(generate_ai_summary "test-doghouse" "$test_data")
    
    echo "$summary"
    echo ""
    save_woof_analysis "test-doghouse" "$test_data" "$summary"
}

# Main function
main() {
    case "${1:-}" in
        "test")
            test_woof_analysis
            ;;
        "latest")
            analyze_latest_push
            ;;
        "")
            analyze_latest_push
            ;;
        *)
            analyze_push_file "$1"
            ;;
    esac
}

# Run main if called as script
if [ "$0" = "./plugins/woof-ai-summary.sh" ] || [ "$0" = "plugins/woof-ai-summary.sh" ]; then
    main "$@"
fi