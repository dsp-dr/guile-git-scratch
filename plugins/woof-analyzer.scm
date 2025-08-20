#!/usr/bin/env guile3
!# 
;; 🐕 Woof Analyzer - Server Integration for AI Summaries
;;
;; This script integrates with the main Git server to provide
;; AI-powered commit analysis via Ollama

(use-modules (ice-9 format)
             (ice-9 rdelim)
             (ice-9 ftw)
             (srfi srfi-1))

(load "plugins/ollama-qwen-changeset-woofs.scm")

(define (analyze-push-file push-file-path)
  "Analyze a push file and generate AI summary"
  (if (file-exists? push-file-path)
      (let* ((push-data (call-with-input-file push-file-path
                         (lambda (port) (read-delimited "" port))))
             (repo-name (extract-repo-from-filename push-file-path))
             (summary (summarize-changeset repo-name push-data)))
        (save-woof-analysis repo-name push-data summary)
        (format #t "🐕 WOOF! Generated AI analysis for ~a~%" repo-name))
      (format #t "🐕 BARK! Push file not found: ~a~%" push-file-path)))

(define (extract-repo-from-filename filename)
  "Extract repository name from push filename"
  (let* ((basename (basename filename))
         (parts (string-split basename #\-)))
    (if (>= (length parts) 3)
        (string-join (drop (take parts (- (length parts) 1)) 2) "-")
        "unknown-repo")))

(define (analyze-latest-push)
  "Find and analyze the most recent push"
  (let* ((pushes-dir "data/pushes")
         (push-files (if (file-exists? pushes-dir)
                        (let ((files (scandir pushes-dir)))
                          (filter (lambda (f) (string-suffix? ".txt" f))
                                 (if files files '())))
                        '())))
    (if (null? push-files)
        (format #t "🐕 No push files found to analyze~%")
        (let ((latest-push (string-append pushes-dir "/" 
                                        (car (sort push-files string>?)))))
          (analyze-push-file latest-push)))))

;; Command line interface
(define (main args)
  (cond
    ((null? (cdr args))
     (format #t "🐕 WOOF! Analyzing latest push...~%")
     (analyze-latest-push))
    ((string=? (cadr args) "test")
     (format #t "🐕 WOOF! Running test analysis...~%")
     (test-ollama-woofs))
    (else
     (format #t "🐕 BARK! Analyzing specific push file...~%")
     (analyze-push-file (cadr args)))))

;; Run if called as script
(when (string-suffix? "woof-analyzer.scm" (car (command-line)))
  (main (command-line)))