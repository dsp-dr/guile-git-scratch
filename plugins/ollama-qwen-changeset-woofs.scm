#!/usr/bin/env guile3
!# 
;; 🐕 Ollama QWen Changeset Woofs - AI-Powered Commit Summaries
;; 
;; This plugin uses Ollama's qwen2.5-coder model to generate humorous
;; dog-themed summaries of Git changesets received by our server.
;;
;; Usage: (summarize-changeset repo-name push-data)
;; Returns: A woofy AI summary of what happened in the push

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (json)
             (srfi srfi-1)
             (srfi srfi-19))

(define *ollama-api-url* "http://localhost:11434/api/generate")
(define *model-name* "qwen2.5-coder:7b")

(define (call-ollama prompt)
  "Call Ollama API with a prompt and return the response"
  (let* ((json-payload (scm->json-string
                        `((model . ,*model-name*)
                          (prompt . ,prompt)
                          (stream . #f)
                          (system . ,(string-append
                                     "You are a friendly dog who works as a Git commit analyst. "
                                     "Always respond with dog-themed humor and puns. "
                                     "Keep responses to 2-3 sentences. "
                                     "Use dog emojis and barking sounds. "
                                     "Focus on what the developer actually did, but make it fun!")))))
         (curl-cmd (format #f "curl -s -X POST ~a -H \"Content-Type: application/json\" -d '~a'"
                          *ollama-api-url* json-payload))
         (pipe (open-input-pipe curl-cmd))
         (response (read-string pipe)))
    (close-pipe pipe)
    (if (string? response)
        (let ((json-response (json-string->scm response)))
          (if (and (hash-table? json-response)
                   (hash-ref json-response "response"))
              (hash-ref json-response "response")
              "🐕 WOOF! AI dog is taking a nap, no summary available!"))
        "🐕 BARK! Couldn't fetch summary from the AI dog!")))

(define (extract-commit-info push-data)
  "Extract meaningful information from push data for AI analysis"
  (let* ((lines (string-split push-data #\newline))
         (request-line (find (lambda (line) 
                              (string-contains line "git-receive-pack"))
                            lines))
         (repo-name (if request-line
                       (let ((parts (string-split request-line #\space)))
                         (if (>= (length parts) 2)
                             (cadr parts)
                             "unknown-repo"))
                       "mystery-repo")))
    (format #f "Repository: ~a~%Push data: ~a" repo-name push-data)))

(define (create-analysis-prompt repo-name push-data)
  "Create a prompt for the AI to analyze the push"
  (format #f 
    "A developer just pushed to repository '~a'. Here's the Git protocol data:

~a

As a friendly dog analyst, explain what this developer accomplished in a fun, dog-themed way. 
Focus on the positive aspects and use plenty of dog puns!"
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
         (test-push-data "git-receive-pack /test-doghouse.git\nhost=localhost:9418")
         (summary (summarize-changeset test-repo test-push-data)))
    (display summary)
    (newline)
    (save-woof-analysis test-repo test-push-data summary)))

;; Export main functions
(define-public summarize-changeset summarize-changeset)
(define-public save-woof-analysis save-woof-analysis)
(define-public test-ollama-woofs test-ollama-woofs)

;; If run as script, do a test
(when (string=? (car (command-line)) 
                (string-append (getcwd) "/plugins/ollama-qwen-changeset-woofs.scm"))
  (display "🐕 TESTING OLLAMA WOOFS...") (newline)
  (test-ollama-woofs))