#!/usr/bin/env guile3
!#

(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (srfi srfi-64)
             (core repository)
             (ice-9 ftw))

(test-begin "repository-tests")

(test-group "Repository initialization"
  (let ((test-repo "/tmp/test-repo-guile-git"))
    ;; Clean up if exists
    (system* "rm" "-rf" test-repo)
    
    (test-assert "Initialize repository"
      (string? (init-repository test-repo)))
    
    (test-assert "Repository has .git directory"
      (file-exists? (string-append test-repo "/.git")))
    
    (test-assert "Repository has HEAD file"
      (file-exists? (string-append test-repo "/.git/HEAD")))
    
    (test-equal "HEAD points to main branch"
      "ref: refs/heads/main"
      (string-trim-both
       (call-with-input-file (string-append test-repo "/.git/HEAD")
         (lambda (port) (read-line port)))))
    
    (test-assert "Repository has objects directory"
      (file-exists? (string-append test-repo "/.git/objects")))
    
    (test-assert "Repository has refs/heads directory"
      (file-exists? (string-append test-repo "/.git/refs/heads")))
    
    ;; Clean up
    (system* "rm" "-rf" test-repo)))

(test-group "Bare repository"
  (let ((test-repo "/tmp/test-bare-repo-guile-git"))
    (system* "rm" "-rf" test-repo)
    
    (test-assert "Initialize bare repository"
      (string? (init-repository test-repo #:bare #t)))
    
    (test-assert "Bare repo has objects directory"
      (file-exists? (string-append test-repo "/objects")))
    
    (test-assert "Bare repo has HEAD file"
      (file-exists? (string-append test-repo "/HEAD")))
    
    ;; Clean up
    (system* "rm" "-rf" test-repo)))

(test-group "References"
  (let ((test-repo "/tmp/test-ref-repo-guile-git"))
    (system* "rm" "-rf" test-repo)
    (init-repository test-repo)
    
    (let ((git-dir (string-append test-repo "/.git")))
      (test-equal "Write and read ref"
        "abc123def456"
        (begin
          (write-ref git-dir "refs/heads/test" "abc123def456")
          (read-ref git-dir "refs/heads/test")))
      
      (test-equal "Read non-existent ref"
        #f
        (read-ref git-dir "refs/heads/nonexistent"))
      
      (test-assert "Update ref"
        (begin
          (update-ref git-dir "refs/heads/test" "newsha123")
          (equal? "newsha123" (read-ref git-dir "refs/heads/test")))))
    
    ;; Clean up
    (system* "rm" "-rf" test-repo)))

(test-end "repository-tests")