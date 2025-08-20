#!/usr/bin/env guile3
!# 
;; 🐕 Ollama Simple Woofs - AI-Powered Commit Summaries (JSON-free version)
;; 
;; This plugin uses Ollama's qwen2.5-coder model to generate humorous
;; dog-themed summaries of Git changesets received by our server.
;; Uses shell commands instead of JSON modules for compatibility

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (srfi srfi-19))

(define *ollama-api-url* "http://localhost:11434/api/generate")
(define *model-name* "qwen2.5-coder:7b")

(define (escape-json-string str)
  "Simple JSON string escaping"
  (string-map (lambda (c)
                (case c
                  ((#\\) #\\)
                  ((#\") #\')  ; Replace quotes with apostrophes for simplicity
                  ((#\newline) #\space)
                  ((#\return) #\space)
                  (else c)))
              str))

(define (call-ollama prompt)
  "Call Ollama API with a prompt and return the response"
  (let* ((escaped-prompt (escape-json-string prompt))
         (system-prompt (escape-json-string
                        (string-append
                         "You are a friendly dog who works as a Git commit analyst. "
                         "Always respond with dog-themed humor and puns. "
                         "Keep responses to 2-3 sentences. "
                         "Use dog emojis and barking sounds. "
                         "Focus on what the developer actually did, but make it fun!")))
         (json-payload (format #f 
                              "{\"model\":\"~a\",\"prompt\":\"~a\",\"system\":\"~a\",\"stream\":false}"
                              *model-name* escaped-prompt system-prompt))
         (curl-cmd (format #f "echo '~a' | curl -s -X POST ~a -H 'Content-Type: application/json' -d @-"
                          json-payload *ollama-api-url*))
         (pipe (open-input-pipe curl-cmd))
         (response (read-delimited "" pipe)))
    (close-pipe pipe)
    (if (and (string? response) (> (string-length response) 0))
        (extract-response-text response)
        "🐕 WOOF! AI dog is taking a nap, no summary available!")))

(define (extract-response-text json-response)
  "Extract the response text from Ollama's JSON response"
  (let ((response-start (string-contains json-response "\"response\":\""))
        (done-marker (string-contains json-response "\"done\":true")))
    (if response-start
        (let* ((text-start (+ response-start 12))  ; Length of "response":"
               (text-end (or done-marker (string-length json-response)))
               (raw-text (substring json-response text-start text-end)))
          ;; Simple cleanup
          (string-trim-both 
           (string-map (lambda (c) (if (eq? c #\\) #\space c)) raw-text)
           (lambda (c) (or (eq? c #\") (eq? c #\,) (eq? c #\})))))
        "🐕 BARK! Couldn't parse AI response!")))

(define (create-analysis-prompt repo-name push-data)
  "Create a prompt for the AI to analyze the push"
  (format #f 
    "A developer just pushed to repository '~a'. Here is the Git protocol data: ~a. As a friendly dog analyst, explain what this developer accomplished in a fun, dog-themed way with plenty of dog puns!"
    repo-name push-data))

(define (summarize-changeset repo-name push-data)
  "Generate an AI-powered summary of a Git push"
  (let* ((timestamp (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
         (prompt (create-analysis-prompt repo-name push-data))
         (ai-summary (call-ollama prompt))
         (header (format #f "🐕 AI WOOF ANALYSIS - ~a" timestamp)))
    (format #f "~a~%~%~a~%" header ai-summary)))

(define (save-woof-analysis repo-name push-data summary)
  "Save the AI analysis to a woof file"
  (let* ((timestamp (date->string (current-date) "~Y~m~d-~H~M~S"))
         (filename (format #f "data/woofs/~a-~a-woof.txt" timestamp repo-name)))
    (call-with-output-file filename
      (lambda (port)
        (format port "=== 🐕 OLLAMA WOOF ANALYSIS ===~%")
        (format port "Repository: ~a~%" repo-name)
        (format port "Timestamp: ~a~%" timestamp)
        (format port "Model: ~a~%~%" *model-name*)
        (format port "~a~%" summary)
        (format port "~%=== Original Push Data ===~%")
        (format port "~a~%" push-data)))
    (format #t "🐕 WOOF! Saved AI analysis to: ~a~%" filename)))

(define (test-ollama-woofs)
  "Test the Ollama integration with a sample push"
  (let* ((test-repo "test-doghouse")
         (test-push-data "git-receive-pack /test-doghouse.git host=localhost:9418")
         (summary (summarize-changeset test-repo test-push-data)))
    (display summary)
    (newline)
    (save-woof-analysis test-repo test-push-data summary)))

;; Export main functions
(define-public summarize-changeset summarize-changeset)
(define-public save-woof-analysis save-woof-analysis)
(define-public test-ollama-woofs test-ollama-woofs)

;; If run as script, do a test
(when (string-suffix? "ollama-simple-woofs.scm" (car (command-line)))
  (display "🐕 TESTING OLLAMA SIMPLE WOOFS...") (newline)
  (test-ollama-woofs))