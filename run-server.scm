#!/usr/bin/env guile3
!#

;; Guile Git Server Runner

(add-to-load-path (string-append (dirname (current-filename)) "/src"))

(use-modules (server server)
             (ice-9 format)
             (ice-9 getopt-long))

(define option-spec
  '((port (single-char #\p) (value #t))
    (repos (single-char #\r) (value #t))
    (help (single-char #\h) (value #f))))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (port (string->number (option-ref options 'port "9418")))
         (repos-path (option-ref options 'repos "./repos")))
    
    (when help-wanted
      (format #t "Usage: ~a [OPTIONS]~%" (car args))
      (format #t "Start a minimal Git server~%~%")
      (format #t "Options:~%")
      (format #t "  -p, --port PORT    Port to listen on (default: 9418)~%")
      (format #t "  -r, --repos PATH   Repository path (default: ./repos)~%")
      (format #t "  -h, --help         Display this help~%")
      (exit 0))
    
    (format #t "=== Guile Git Server v0.1.0 ===~%")
    (format #t "Minimal Git server implementation in Guile~%~%")
    
    ;; Start server
    (start-git-server #:port port #:repos-path repos-path)))

(main (command-line))