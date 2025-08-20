(define-module (core repository)
  #:use-module (core objects)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:export (init-repository
            find-repository
            write-ref
            read-ref
            update-ref))

(define* (init-repository path #:key (bare #f))
  "Initialize a new Git repository"
  (let ((git-dir (if bare path (string-append path "/.git"))))
    ;; Create directory structure
    (for-each (lambda (dir)
                (system* "mkdir" "-p" (string-append git-dir "/" dir)))
              '("" "objects" "refs" "refs/heads" "refs/tags" 
                "hooks" "info" "branches"))
    
    ;; Write HEAD
    (call-with-output-file (string-append git-dir "/HEAD")
      (lambda (port)
        (display "ref: refs/heads/main\n" port)))
    
    ;; Write config
    (call-with-output-file (string-append git-dir "/config")
      (lambda (port)
        (display "[core]\n" port)
        (format port "\trepositoryformatversion = 0\n")
        (format port "\tfilemode = true\n")
        (format port "\tbare = ~a\n" (if bare "true" "false"))))
    
    ;; Write description
    (call-with-output-file (string-append git-dir "/description")
      (lambda (port)
        (display "Unnamed repository; edit this file to name it.\n" port)))
    
    git-dir))

(define (find-repository #:optional (path (getcwd)))
  "Find the .git directory for the current repository"
  (let loop ((current path))
    (cond 
     ((string=? current "/") #f)
     ((file-exists? (string-append current "/.git"))
      (string-append current "/.git"))
     ((and (file-exists? current)
           (file-exists? (string-append current "/config"))
           (file-exists? (string-append current "/objects")))
      current) ; Bare repository
     (else 
      (loop (dirname current))))))

(define (write-ref repo-path ref-name sha)
  "Write a reference"
  (let* ((ref-path (string-append repo-path "/" ref-name)))
    ;; Create parent directory if needed
    (let ((dir (dirname ref-path)))
      (when (not (file-exists? dir))
        (system* "mkdir" "-p" dir)))
    ;; Write ref
    (call-with-output-file ref-path
      (lambda (port)
        (format port "~a\n" sha)))))

(define (read-ref repo-path ref-name)
  "Read a reference"
  (let ((ref-path (string-append repo-path "/" ref-name)))
    (if (file-exists? ref-path)
        (string-trim-both 
         (call-with-input-file ref-path get-string-all))
        #f)))

(define* (update-ref repo-path ref-name new-sha #:key (old-sha #f))
  "Update a reference, optionally checking old value"
  (if old-sha
      (let ((current (read-ref repo-path ref-name)))
        (if (equal? current old-sha)
            (write-ref repo-path ref-name new-sha)
            (error "Reference has changed" ref-name current old-sha)))
      (write-ref repo-path ref-name new-sha)))

(define (dirname path)
  "Get directory name from path"
  (let ((slash-pos (string-rindex path #\/)))
    (if slash-pos
        (substring path 0 slash-pos)
        ".")))