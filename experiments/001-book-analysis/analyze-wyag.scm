#!/usr/bin/env guile3
!#

(use-modules (ice-9 format)
             (ice-9 regex)
             (ice-9 textual-ports))

(define wyag-file "../../tmp/wyag.html")

(define (extract-commands html)
  (let ((cmd-regex "<li><a href=\"#cmd-([^\"]+)\">.*?The ([^ ]+) command</a></li>"))
    (let loop ((pos 0) (commands '()))
      (let ((match (string-match cmd-regex html pos)))
        (if match
            (loop (match:end match)
                  (cons (list (match:substring match 2)
                             (match:substring match 1))
                        commands))
            (reverse commands))))))

(define (extract-sections html)
  (let ((section-regex "<li><a href=\"#([^\"]+)\">.*?([^<]+)</a>"))
    (let loop ((pos 0) (sections '()))
      (let ((match (string-match section-regex html pos)))
        (if match
            (let ((id (match:substring match 1))
                  (title (match:substring match 2)))
              (if (and (not (string-prefix? "org" id))
                      (not (string-prefix? "cmd-" id)))
                  (loop (match:end match)
                        (cons (cons id title) sections))
                  (loop (match:end match) sections)))
            (reverse sections))))))

(format #t "=== WYAG Book Analysis ===~%~%")

(if (file-exists? wyag-file)
    (let ((html (call-with-input-file wyag-file get-string-all)))
      (format #t "Commands to implement:~%")
      (for-each
       (lambda (cmd)
         (format #t "  - ~a (section: ~a)~%" (car cmd) (cadr cmd)))
       (extract-commands html))
      
      (newline)
      (format #t "Major sections:~%")
      (for-each
       (lambda (section)
         (format #t "  - ~a: ~a~%" (car section) (cdr section)))
       (take (extract-sections html) 15)))
    (format #t "Error: WYAG file not found at ~a~%" wyag-file))

(newline)
(format #t "=== Analysis Complete ===~%")