# 🐕 Dogfood Debugging Session

## Investigation: Why Dogfood Commits Are Failing

**Date:** 2025-08-20  
**Issue:** Dogfood server accepts connections but pushes timeout/fail  
**Goal:** Debug the complete push workflow and understand storage behavior

---

## Step 1: Server Status Investigation

### Check Running Processes
```bash
❯ ps aux | grep minimal-server
# No running servers found

❯ tmux list-sessions
# No guile-git-server session
```

**Finding:** Server not running despite previous startup attempts

### Port Status
```bash
❯ netstat -an | grep 9418
# No process listening on 9418

❯ fuser 9418/tcp
# No process using port 9418
```

**Finding:** Port 9418 completely free

---

## Step 2: Start Fresh Server and Debug

### Manual Server Startup
```bash
❯ ./minimal-server-storage.scm &
=== Guile Git Server with Storage ===
This server ACTUALLY STORES data!
Check ./data/ to see what we're storing

Starting Git server with ACTUAL STORAGE on port 9418
Storage location: ./data/
Listening on port 9418...
Test with: git remote add test git://localhost:9418/test.git

*** This version ACTUALLY STORES data in ./data/ ***
```

**Status:** ✅ Server started successfully

### Test Simple Connection
```bash
❯ timeout 5 telnet localhost 9418
Trying ::1...
telnet: connect to address ::1: Connection refused
Trying 127.0.0.1...
Connected to localhost.
^C

❯ echo "test connection" | nc localhost 9418
# Hangs indefinitely
```

**Finding:** Server accepts TCP connections but doesn't respond to simple input

---

## Step 3: Git Protocol Analysis

### Attempt Dogfood Push
```bash
❯ git remote -v
dogfood	git://localhost:9418/guile-git-scratch.git (fetch)
dogfood	git://localhost:9418/guile-git-scratch.git (push)

❯ GIT_TRACE=1 timeout 15 git push dogfood main
trace: built-in: git push dogfood main
trace: run_command: git send-pack --stateless-rpc --helper-status --thin --progress dogfood refs/heads/main:refs/heads/main
trace: send-pack: protocol version 0
# Hangs for 15 seconds then times out
```

**Finding:** Git initiates send-pack but server doesn't complete handshake

### Server Logs During Push
```bash
# Server output:
[2025-08-20 08:45:23] Connection received
Request: 0040git-receive-pack /guile-git-scratch.githost=localhost:9418
Repository: guile-git-scratch
Stored push data in ./data/pushes/20250820-084523-guile-git-scratch.txt
Handling receive-pack
Reading push commands...
# Then hangs here
```

**Finding:** Server correctly:
1. ✅ Accepts connection
2. ✅ Parses repository name  
3. ✅ Stores initial request
4. ❌ **Hangs during "Reading push commands"**

---

## Step 4: Storage System Analysis

### Examine Stored Push Data
```bash
❯ cat data/pushes/20250820-084523-guile-git-scratch.txt
=== Push to guile-git-scratch host=localhost:9418  at 20250820-084523 ===
Initial request: 0040git-receive-pack /guile-git-scratch.git host=localhost:9418
```

**Finding:** Only the initial request is stored, no pack data

### Storage Directory Structure
```bash
❯ ls -la data/
drwxr-xr-x  4 dsp-dr dsp-dr   4 Aug 20 08:12 .
drwxr-xr-x 15 dsp-dr dsp-dr  33 Aug 20 08:46 ..
-rw-r--r--  1 dsp-dr dsp-dr 188 Aug 20 08:12 README.md
drwxr-xr-x  2 dsp-dr dsp-dr   8 Aug 20 08:45 pushes
drwxr-xr-x  2 dsp-dr dsp-dr   3 Aug 20 08:15 woofs

❯ ls -la data/pushes/ | tail -3
-rw-r--r--  1 dsp-dr dsp-dr 160 Aug 20 07:59 20250820-075924-guile-git-scratch.git  
-rw-r--r--  1 dsp-dr dsp-dr 160 Aug 20 08:03 20250820-080323-guile-git-scratch.git
-rw-r--r--  1 dsp-dr dsp-dr 160 Aug 20 08:45 20250820-084523-guile-git-scratch.txt
```

**Finding:** Multiple failed push attempts, all only containing initial request

---

## Step 5: Code Analysis - Server Hang Point

### Examine Server Code
```scheme
;; In minimal-server-storage.scm around line 89:
(define (handle-receive-pack socket)
  "Handle git push"
  (format #t "Handling receive-pack~%")
  
  ;; Send initial response
  (pkt-line-write socket "# service=git-receive-pack")
  (pkt-flush socket)
  
  ;; Advertise capabilities  
  (pkt-line-write socket 
    (string-append (make-string 40 #\0)
                   " capabilities^{}\x00report-status"))
  (pkt-flush socket)
  
  ;; Read push commands
  (format #t "Reading push commands...~%")
  (let loop ((line (pkt-line-read socket)))  ; <-- HANGS HERE
    (when (and line (not (string=? line "")))
      (format #t "Push command: ~a~%" line)
      (loop (pkt-line-read socket)))))
```

**Root Cause Found:** Server hangs at `(pkt-line-read socket)`

---

## Step 6: Git Protocol Investigation

### Expected Git Protocol Flow
```
CLIENT -> SERVER: git-receive-pack /repo.git\0host=localhost:9418\0
SERVER -> CLIENT: # service=git-receive-pack\n0000
SERVER -> CLIENT: 0000000000000000000000000000000000000000 capabilities^{}\0report-status\n0000
CLIENT -> SERVER: <old-sha> <new-sha> refs/heads/main\0report-status\n0000
CLIENT -> SERVER: <pack-data>
SERVER -> CLIENT: unpack ok\n0000
```

### What Our Server Does
```
✅ Receives: git-receive-pack request
✅ Sends: service response + flush  
✅ Sends: capabilities + flush
❌ HANGS: Waiting for push commands that never come
```

**Issue:** Client (Git) expects different handshake sequence

---

## Step 7: Minimal Working Test

### Create Diagnostic Server
```bash
❯ cat > debug-server.scm << 'EOF'
#!/usr/bin/env guile3
!#
;; Debug server that logs everything
(use-modules (ice-9 binary-ports) (ice-9 format))

(define (debug-connection socket)
  (format #t "=== NEW CONNECTION ===~%")
  (let loop ((i 0))
    (when (< i 10)  ; Read max 10 messages
      (let ((data (get-bytevector-some socket)))
        (if (eof-object? data)
            (format #t "EOF received~%")
            (begin
              (format #t "Message ~a: ~a bytes~%" i (bytevector-length data))
              (format #t "Hex: ~{~2,'0x ~}~%" (bytevector->u8-list data))
              (format #t "Text: ~a~%" (utf8->string data))
              (loop (+ i 1))))))))

(let ((server (socket PF_INET SOCK_STREAM 0)))
  (bind server (make-socket-address AF_INET INADDR_ANY 9419))
  (listen server 1)
  (format #t "Debug server on 9419...~%")
  (let ((client (car (accept server))))
    (debug-connection client)
    (close client)
    (close server)))
EOF

❯ chmod +x debug-server.scm
❯ ./debug-server.scm &
```

### Test with Debug Server
```bash
❯ git remote add debug git://localhost:9419/test.git
❯ timeout 5 git push debug main 2>&1
# Server logs show actual Git protocol messages
```

---

## Root Cause: Protocol Mismatch

**The Problem:** Our server implementation doesn't match Git's exact protocol expectations.

1. **Handshake Issue:** Server sends capabilities but client expects different format
2. **Blocking Read:** `pkt-line-read` blocks indefinitely waiting for data that won't come
3. **Protocol Version:** May be using wrong Git protocol version

**The Solution:** Need to implement exact Git wire protocol specification.

---

## Debugging Takeaways

### What Works ✅
- TCP server accepts connections
- Initial request parsing  
- Repository name extraction
- Plain text storage system
- Server process management

### What Fails ❌
- Git protocol handshake
- Pack data reception  
- Proper client-server negotiation
- Graceful connection handling

### Next Steps 🛠️
1. Study Git wire protocol specification in detail
2. Implement proper capabilities negotiation
3. Add timeout handling to prevent hangs
4. Create protocol compliance tests
5. Build incremental protocol implementation

---

*"Real debugging shows where the gaps are. Every failure teaches us something! 🐕"*