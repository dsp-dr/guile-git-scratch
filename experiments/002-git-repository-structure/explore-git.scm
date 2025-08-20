#!/usr/bin/env guile3
!#

(use-modules (ice-9 format)
             (ice-9 ftw)
             (ice-9 textual-ports))

(define test-repo ".git")

(define (explore-directory dir prefix)
  (let ((items (scandir dir)))
    (for-each
     (lambda (item)
       (unless (member item '("." ".."))
         (let ((path (string-append dir "/" item)))
           (format #t "~a~a~%" prefix item)
           (when (and (file-is-directory? path)
                     (< (string-length prefix) 8))
             (explore-directory path (string-append prefix "  "))))))
     (sort items string<?))))

(format #t "=== Git Repository Structure ===~%~%")

(if (file-exists? test-repo)
    (begin
      (format #t "Structure of .git directory:~%")
      (explore-directory test-repo "")
      
      (newline)
      (format #t "Key files:~%")
      
      (when (file-exists? ".git/config")
        (format #t "~%Config file:~%")
        (call-with-input-file ".git/config"
          (lambda (port)
            (let loop ((line (read-line port)))
              (unless (eof-object? line)
                (format #t "  ~a~%" line)
                (loop (read-line port)))))))
      
      (when (file-exists? ".git/HEAD")
        (format #t "~%HEAD file:~%")
        (format #t "  ~a~%" 
                (call-with-input-file ".git/HEAD" get-string-all))))
    (format #t "No .git directory found. Run in a git repository.~%"))

(newline)
(format #t "=== Exploration Complete ===~%")