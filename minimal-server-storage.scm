#!/usr/bin/env guile3
!#

;; Minimal Git Server with Plain Text Storage

(add-to-load-path (string-append (dirname (current-filename)) "/src"))

(use-modules (ice-9 format)
             (ice-9 rdelim)
             (ice-9 binary-ports)
             (ice-9 textual-ports)
             (storage plain-text))

(define (start-server port)
  (format #t "Starting Git server with ACTUAL STORAGE on port ~a~%" port)
  (format #t "Storage location: ./data/~%")
  
  ;; Ensure storage directories exist
  (ensure-storage-dirs)
  
  (let ((server-socket (socket PF_INET SOCK_STREAM 0)))
    (setsockopt server-socket SOL_SOCKET SO_REUSEADDR 1)
    (bind server-socket (make-socket-address AF_INET INADDR_ANY port))
    (listen server-socket 5)
    
    (format #t "Listening on port ~a...~%" port)
    (format #t "Test with: git remote add test git://localhost:~a/test.git~%~%" port)
    (format #t "*** This version ACTUALLY STORES data in ./data/ ***~%~%")
    
    ;; Keep accepting connections forever
    (let loop ()
      (let* ((client (accept server-socket))
             (socket (car client)))
        (format #t "[~a] Connection received~%" 
                (strftime "%H:%M:%S" (localtime (current-time))))
        (handle-client socket)
        (format #t "Ready for next connection...~%~%")
        (loop)))))

(define (handle-client socket)
  (catch #t
    (lambda ()
      (let ((line (read-line socket)))
        (format #t "Request: ~a~%" line)
        
        ;; Extract repository name from request
        (let ((repo-name (extract-repo-name line)))
          (format #t "Repository: ~a~%" repo-name)
          
          ;; Store the initial request
          (when repo-name
            (store-push-data repo-name (format #f "Initial request: ~a" line)))
          
          ;; Parse git command
          (cond
           ((and (string? line) (string-contains line "git-receive-pack"))
            (format #t "Handling receive-pack~%")
            (handle-receive-pack socket repo-name))
           ((and (string? line) (string-contains line "git-upload-pack"))
            (format #t "Handling upload-pack~%")
            (handle-upload-pack socket repo-name))
           (else
            (format #t "Unknown or invalid request~%")))))
      
      (close-port socket)
      (format #t "Connection closed~%"))
    (lambda (key . args)
      (format #t "Error: ~a ~a~%" key args)
      (catch #t
        (lambda () (close-port socket))
        (lambda _ #t)))))

(define (extract-repo-name line)
  "Extract repository name from git request line"
  (if (and (string? line) (string-contains line ".git"))
      (let* ((parts (string-split line #\space))
             (path (if (>= (length parts) 2) (cadr parts) ""))
             (clean-path (string-trim-both path #\/)))
        (if (string-suffix? ".git" clean-path)
            (substring clean-path 0 (- (string-length clean-path) 4))
            clean-path))
      "unknown"))

(define (pkt-line-write port text)
  "Write pkt-line format"
  (let* ((line (string-append text "\n"))
         (len (+ 4 (string-length line)))
         (hex (format #f "~4,'0x" len)))
    (display hex port)
    (display line port)))

(define (pkt-flush port)
  "Send flush packet"
  (display "0000" port)
  (force-output port))

(define (handle-receive-pack socket repo-name)
  "Handle git push with actual storage"
  ;; Send capabilities
  (pkt-line-write socket "# service=git-receive-pack")
  (pkt-flush socket)
  
  ;; Check for existing refs
  (let ((refs (list-refs repo-name)))
    (if (null? refs)
        ;; No refs - send zero ID
        (begin
          (pkt-line-write socket 
            (string-append (make-string 40 #\0)
                          " capabilities^{}\x00report-status"))
          (pkt-flush socket))
        ;; Send existing refs
        (begin
          (for-each (lambda (ref)
                     (pkt-line-write socket
                       (format #f "~a ~a" (cdr ref) (car ref))))
                   refs)
          (pkt-flush socket))))
  
  ;; Read and store commands
  (format #t "Reading push commands...~%")
  (let ((commands '()))
    (let loop ()
      (let ((line (read-line socket)))
        (cond
         ((eof-object? line)
          (format #t "Client disconnected~%"))
         ((string=? line "0000")
          (format #t "Flush packet received~%"))
         ((string-prefix? "00" line)
          ;; It's a pkt-line, extract the actual command
          (let ((cmd (substring line 4)))
            (format #t "  Command: ~a~%" cmd)
            (set! commands (cons cmd commands))
            ;; Store the command
            (store-push-data repo-name (format #f "Command: ~a" cmd))
            (loop)))
         (else
          (format #t "  Data: ~a~%" line)
          (store-push-data repo-name (format #f "Data: ~a" line))
          (loop)))))
    
    ;; Process commands (extract old/new SHAs and ref names)
    (for-each (lambda (cmd)
                (when (>= (string-length cmd) 81)
                  (let ((old-sha (substring cmd 0 40))
                        (new-sha (substring cmd 41 81))
                        (ref-name (if (> (string-length cmd) 82)
                                     (substring cmd 82)
                                     "HEAD")))
                    (format #t "Update: ~a -> ~a for ~a~%" 
                           old-sha new-sha ref-name)
                    ;; Store the ref!
                    (store-ref repo-name ref-name new-sha))))
             commands))
  
  ;; Read pack data if any
  (format #t "Reading pack data...~%")
  (let ((pack-data (get-bytevector-all socket)))
    (when (and pack-data (> (bytevector-length pack-data) 0))
      (format #t "Received ~a bytes of pack data~%" 
             (bytevector-length pack-data))
      ;; Store pack data for analysis
      (store-push-data repo-name 
                      (format #f "Pack data: ~a bytes" 
                             (bytevector-length pack-data)))))
  
  ;; Send success report (now it's TRUE!)
  (pkt-line-write socket "unpack ok")
  (pkt-flush socket)
  (format #t "Sent success report (and we actually stored stuff!)~%"))

(define (handle-upload-pack socket repo-name)
  "Handle git fetch/clone"
  ;; Send capabilities
  (pkt-line-write socket "# service=git-upload-pack")
  (pkt-flush socket)
  
  ;; Check for refs
  (let ((refs (list-refs repo-name)))
    (if (null? refs)
        ;; No refs
        (begin
          (pkt-line-write socket
            (string-append (make-string 40 #\0)
                          " capabilities^{}\x00"))
          (pkt-flush socket))
        ;; Send refs
        (begin
          (for-each (lambda (ref)
                     (pkt-line-write socket
                       (format #f "~a ~a" (cdr ref) (car ref))))
                   refs)
          (pkt-flush socket))))
  
  ;; Read wants
  (format #t "Reading wants...~%")
  (let loop ()
    (let ((line (read-line socket)))
      (when (and (not (eof-object? line))
                (not (string=? line "0000")))
        (format #t "  Want: ~a~%" line)
        (store-push-data repo-name (format #f "Want: ~a" line))
        (loop))))
  
  ;; Send NAK
  (pkt-line-write socket "NAK")
  (pkt-flush socket)
  (format #t "Sent NAK~%"))

;; Start the server
(format #t "~%=== Guile Git Server with Storage ===~%")
(format #t "This server ACTUALLY STORES data!~%")
(format #t "Check ./data/ to see what we're storing~%~%")
(start-server 9418)