#!/usr/bin/env guile3
!#

;; Test runner for Guile Git implementation

(use-modules (srfi srfi-64)
             (ice-9 ftw)
             (ice-9 format))

(define (run-test-file file)
  "Run a single test file"
  (format #t "Running ~a...~%" file)
  (load file))

(define (find-test-files dir)
  "Find all test-*.scm files in directory"
  (let ((files '()))
    (ftw dir
         (lambda (filename statinfo flag)
           (when (and (eq? flag 'regular)
                     (string-suffix? ".scm" filename)
                     (string-contains filename "/test-")
                     (not (string-contains filename "/test-runner.scm")))
             (set! files (cons filename files)))
           #t))
    (sort files string<?)))

(define (main)
  (format #t "~%=== Guile Git Test Suite ===~%~%")
  
  (let* ((test-dir (dirname (current-filename)))
         (test-files (find-test-files test-dir))
         (runner (test-runner-current)))
    
    ;; Configure test runner
    (test-runner-current (test-runner-simple))
    
    ;; Run all tests
    (for-each run-test-file test-files)
    
    ;; Summary
    (format #t "~%=== Test Summary ===~%")
    (format #t "Tests run: ~a~%" (test-runner-test-count runner))
    (format #t "Passed: ~a~%" (test-runner-pass-count runner))
    (format #t "Failed: ~a~%" (test-runner-fail-count runner))
    (format #t "Skipped: ~a~%~%" (test-runner-skip-count runner))
    
    ;; Exit with appropriate code
    (exit (if (zero? (test-runner-fail-count runner)) 0 1))))

;; Run if executed directly
(when (equal? (current-filename) (car (command-line)))
  (main))