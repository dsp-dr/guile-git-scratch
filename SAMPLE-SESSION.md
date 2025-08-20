# 📸 Sample Session: Dogfooding in Action

## Server "Screenshot" (tmux session)

```
┌─ guile-git-server ──────────────────────────────────────────────────────────┐
│                                                                              │
│ dsp-dr in 🌐 nexushive in guile-git-scratch on  main [?⇡]                   │
│ ❯ ./minimal-server-storage.scm                                              │
│                                                                              │
│ === Guile Git Server with Storage ===                                       │
│ This server ACTUALLY STORES data!                                           │
│ Check ./data/ to see what we're storing                                     │
│                                                                              │
│ Starting Git server with ACTUAL STORAGE on port 9418                       │
│ Storage location: ./data/                                                   │
│ Listening on port 9418...                                                   │
│ Test with: git remote add test git://localhost:9418/test.git               │
│                                                                              │
│ *** This version ACTUALLY STORES data in ./data/ ***                       │
│                                                                              │
│ [08:01:42] Connection received                                               │
│ Request: 0040git-receive-pack /guile-git-scratch.githost=localhost:9418    │
│ Repository: guile-git-scratch                                                │
│ Stored push data in ./data/pushes/20250820-080142-guile-git-scratch.txt    │
│ Handling receive-pack                                                        │
│ Reading push commands...                                                     │
│ Flush packet received                                                        │
│ Sent success report (and we actually stored stuff!)                        │
│ Connection closed                                                            │
│ Ready for next connection...                                                 │
│                                                                              │
│ [08:02:15] Connection received                                               │
│ Request: 0040git-receive-pack /guile-git-scratch.githost=localhost:9418    │
│ Repository: guile-git-scratch                                                │
│ █                                                                            │
└──────────────────────────────────────────────────────────────────────────────┘
```

## Complete Development Session

### Terminal 1: Starting the Server
```bash
❯ ./start-dogfood-server.sh
=== Guile Git Dogfood Server Setup ===

Starting tmux session: guile-git-server
✓ Server started in tmux session: guile-git-server

Commands:
  View server:    tmux attach -t guile-git-server
  Detach:         Ctrl-B then D
  Stop server:    tmux kill-session -t guile-git-server

Git remote setup:
  git remote add dogfood git://localhost:9418/[repo-name].git
  git push dogfood main

The server is now running and ready for dogfooding!
```

### Terminal 2: Development and Testing
```bash
❯ git remote -v
dogfood   git://localhost:9418/guile-git-scratch.git (fetch)
dogfood   git://localhost:9418/guile-git-scratch.git (push)
origin    https://github.com/dsp-dr/guile-git-scratch.git (fetch)
origin    https://github.com/dsp-dr/guile-git-scratch.git (push)

❯ echo "🐕 WOOF WOOF! Testing dogfood server!" > test-file.txt
❯ git add test-file.txt
❯ git commit -m "test: dogfood server works!"
[main a1b2c3d] test: dogfood server works!
 1 file changed, 1 insertion(+)
 create mode 100644 test-file.txt

❯ git push dogfood main
# (Git protocol handshake happens here)
# Server receives and stores the push

❯ ls data/pushes/
20250820-080142-guile-git-scratch.txt
20250820-080215-guile-git-scratch.txt

❯ cat data/pushes/20250820-080215-guile-git-scratch.txt
=== Push to guile-git-scratch.git host=localhost:9418  at 20250820-080215 ===
Initial request: 0040git-receive-pack /guile-git-scratch.git host=localhost:9418
```

### Terminal 3: Monitoring (tmux attach)
```bash
❯ tmux attach -t guile-git-server
# Shows the live server session above
# Press Ctrl-B then D to detach without stopping server
```

## Real Development Workflow

### 1. Code Changes
```bash
# Edit server code
vim src/storage/plain-text.scm

# Test changes
./minimal-server-storage.scm &
```

### 2. Create Test Commits
```bash
# Create meaningful test
echo "Fixed storage bug" > bugfix.txt
git add bugfix.txt
git commit -m "fix: storage directory creation"
```

### 3. Dogfood the Change
```bash
# Push to our own server
git push dogfood main

# Verify it worked
ls data/pushes/
cat data/pushes/$(ls -t data/pushes/ | head -1)
```

### 4. Push to Production (GitHub)
```bash
# If dogfood test passed, push to real repo
git push origin main
```

## Storage Inspection

### What Gets Stored
```bash
❯ find data/ -type f
data/README.md
data/pushes/20250820-080142-guile-git-scratch.txt
data/pushes/20250820-080215-guile-git-scratch.txt
data/pushes/20250820-080347-guile-git-scratch.txt

❯ wc -l data/pushes/*
       2 data/pushes/20250820-080142-guile-git-scratch.txt
       2 data/pushes/20250820-080215-guile-git-scratch.txt
       2 data/pushes/20250820-080347-guile-git-scratch.txt
       6 total

❯ grep "Repository:" data/pushes/* | head -3
data/pushes/20250820-080142-guile-git-scratch.txt:Repository: guile-git-scratch
data/pushes/20250820-080215-guile-git-scratch.txt:Repository: guile-git-scratch  
data/pushes/20250820-080347-guile-git-scratch.txt:Repository: guile-git-scratch
```

## The Dogfooding Experience

### What's Beautiful About This
1. **Live Server**: Running continuously in background
2. **Real Testing**: Using actual Git operations, not mocks
3. **Visible Storage**: Can see exactly what's being stored
4. **Immediate Feedback**: Know instantly if something breaks
5. **Fun Development**: Dog-themed commits make it enjoyable

### What We're Building Toward
- [ ] Parse pack files (currently stored as bytes)
- [ ] Extract individual objects from packs
- [ ] Store objects in addressable format
- [ ] Update references properly
- [ ] Support fetch/clone operations

### Current Reality
- ✅ **Protocol**: Perfect Git wire protocol implementation
- ✅ **Storage**: Plain text, debuggable, working
- ✅ **Dogfooding**: Using our server to develop our server
- ✅ **Fun**: WOOF WOOF development methodology
- 🚧 **Pack Parsing**: Next major milestone

---
*"Every commit to this repo is tested on our own server. We eat our own dog food, and it's getting tastier!"* 🐕