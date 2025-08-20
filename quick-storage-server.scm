#!/usr/bin/env guile3
!#

;; Quick Storage Server - Non-blocking version for testing
;; Based on minimal-server-storage.scm but with better error handling

(add-to-load-path (string-append (dirname (current-filename)) "/src"))

(use-modules (ice-9 format)
             (ice-9 rdelim) 
             (ice-9 binary-ports)
             (ice-9 textual-ports)
             (storage plain-text))

(define *server-socket* #f)

(define (cleanup-and-exit)
  "Clean shutdown"
  (when *server-socket*
    (close *server-socket*))
  (format #t "🐕 Server shut down cleanly!~%")
  (exit 0))

(define (handle-single-connection port)
  "Handle one connection then exit (for testing)"
  (format #t "=== Quick Storage Server Test ===~%")
  (format #t "This server will handle ONE connection then exit~%")
  (format #t "Storage: ./data/~%~%")
  
  ;; Ensure storage exists
  (ensure-storage-dirs)
  
  (let ((server-socket (socket PF_INET SOCK_STREAM 0)))
    (set! *server-socket* server-socket)
    (setsockopt server-socket SOL_SOCKET SO_REUSEADDR 1)
    (bind server-socket (make-socket-address AF_INET INADDR_ANY port))
    (listen server-socket 1)
    
    (format #t "Listening on port ~a (single connection mode)...~%" port)
    
    ;; Accept one connection
    (let* ((client-socket (car (accept server-socket)))
           (timestamp (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time)))))
      
      (format #t "[~a] Connection received~%" timestamp)
      
      ;; Read request
      (let ((request-line (read-line client-socket)))
        (format #t "Request: ~a~%" request-line)
        
        ;; Store the request
        (when (string? request-line)
          (let ((repo-name (extract-repo-name request-line)))
            (format #t "Repository: ~a~%" repo-name)
            (store-push-data repo-name request-line)
            (format #t "Stored request data~%")))
        
        ;; Send minimal response
        (display "0000" client-socket) ; Empty response
        (force-output client-socket)
        
        (format #t "Response sent~%")
        (close client-socket)
        (format #t "Connection closed~%"))
      
      (close server-socket)
      (format #t "🐕 Single connection test completed!~%"))))

;; Signal handlers
(sigaction SIGINT (lambda (sig) (cleanup-and-exit)))
(sigaction SIGTERM (lambda (sig) (cleanup-and-exit)))

;; Main
(define (main args)
  (let ((port (if (> (length args) 1)
                  (string->number (cadr args))
                  9418)))
    (handle-single-connection port)))

(main (command-line))