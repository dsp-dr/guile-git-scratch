#!/usr/bin/env guile3
!#

;; Simple test runner for Guile Git

(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (srfi srfi-64)
             (ice-9 format))

;; Set up test runner
(test-runner-current (test-runner-simple))

(format #t "~%=== Running Guile Git Test Suite ===~%~%")

;; Load and run test files
(load "test-sha1.scm")
(load "test-repository.scm")
(load "test-objects.scm")

;; Get results
(let ((runner (test-runner-current)))
  (format #t "~%=== Test Summary ===~%")
  (format #t "Passed: ~a~%" (test-runner-pass-count runner))
  (format #t "Failed: ~a~%" (test-runner-fail-count runner))
  (format #t "~%")
  
  ;; Exit with appropriate code
  (exit (if (zero? (test-runner-fail-count runner)) 0 1)))