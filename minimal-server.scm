#!/usr/bin/env guile3
!#

;; Minimal Git Server - Just enough to test pushing

(use-modules (ice-9 format)
             (ice-9 rdelim)
             (ice-9 binary-ports))

(define (start-server port)
  (format #t "Starting minimal Git server on port ~a~%" port)
  
  (let ((server-socket (socket PF_INET SOCK_STREAM 0)))
    (setsockopt server-socket SOL_SOCKET SO_REUSEADDR 1)
    (bind server-socket (make-socket-address AF_INET INADDR_ANY port))
    (listen server-socket 5)
    
    (format #t "Listening on port ~a...~%" port)
    (format #t "Test with: git remote add test git://localhost:~a/test.git~%~%" port)
    
    (let loop ()
      (let* ((client (accept server-socket))
             (socket (car client)))
        (format #t "Connection received~%")
        (handle-client socket)
        (loop)))))

(define (handle-client socket)
  (catch #t
    (lambda ()
      (let ((line (read-line socket)))
        (format #t "Request: ~a~%" line)
        
        ;; Parse git command
        (cond
         ((string-contains line "git-receive-pack")
          (format #t "Handling receive-pack~%")
          (handle-receive-pack socket))
         ((string-contains line "git-upload-pack")
          (format #t "Handling upload-pack~%")
          (handle-upload-pack socket))
         (else
          (format #t "Unknown request~%"))))
      
      (close-port socket))
    (lambda (key . args)
      (format #t "Error: ~a ~a~%" key args)
      (close-port socket))))

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

(define (handle-receive-pack socket)
  "Handle git push"
  ;; Send capabilities
  (pkt-line-write socket "# service=git-receive-pack")
  (pkt-flush socket)
  
  ;; Send zero ref with capabilities
  (pkt-line-write socket 
    (string-append (make-string 40 #\0)
                  " capabilities^{}\x00report-status"))
  (pkt-flush socket)
  
  ;; Read commands
  (let loop ()
    (let ((line (read-line socket)))
      (when (not (eof-object? line))
        (format #t "Received: ~a~%" line)
        (when (not (string=? line "0000"))
          (loop)))))
  
  ;; Send success report
  (pkt-line-write socket "unpack ok")
  (pkt-flush socket))

(define (handle-upload-pack socket)
  "Handle git fetch/clone"
  ;; Send capabilities
  (pkt-line-write socket "# service=git-upload-pack")
  (pkt-flush socket)
  
  ;; Send zero ref
  (pkt-line-write socket
    (string-append (make-string 40 #\0)
                  " capabilities^{}\x00"))
  (pkt-flush socket)
  
  ;; Read wants
  (let loop ()
    (let ((line (read-line socket)))
      (when (and (not (eof-object? line))
                (not (string=? line "0000")))
        (format #t "Want: ~a~%" line)
        (loop))))
  
  ;; Send NAK
  (pkt-line-write socket "NAK")
  (pkt-flush socket))

;; Start the server
(start-server 9418)