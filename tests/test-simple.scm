#!/usr/bin/env guile3
!#

;; Simple tests that don't require SHA-1

(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (srfi srfi-64)
             (core repository)
             (ice-9 format))

(test-runner-current (test-runner-simple))

(format #t "Running simple tests...~%")

(test-begin "simple-tests")

(test-group "Basic functionality"
  (test-assert "Can create test directory"
    (begin
      (system* "mkdir" "-p" "/tmp/guile-git-test")
      (file-exists? "/tmp/guile-git-test")))
  
  (test-equal "String operations"
    "test"
    "test")
  
  (test-assert "Repository module loads"
    (module-defined? (resolve-module '(core repository)) 'init-repository)))

(test-group "Repository basics"
  (let ((test-repo "/tmp/test-simple-repo"))
    (system* "rm" "-rf" test-repo)
    
    (test-assert "Initialize repository"
      (string? (init-repository test-repo)))
    
    (test-assert ".git directory created"
      (file-exists? (string-append test-repo "/.git")))
    
    (system* "rm" "-rf" test-repo)))

(test-end "simple-tests")

(let ((runner (test-runner-current)))
  (format #t "~%Simple tests: ~a passed, ~a failed~%"
          (test-runner-pass-count runner)
          (test-runner-fail-count runner))
  (exit (if (zero? (test-runner-fail-count runner)) 0 1)))