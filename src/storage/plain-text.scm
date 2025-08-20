(define-module (storage plain-text)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-19)  ; for timestamps
  #:export (store-push-data
            store-object
            store-ref
            read-ref
            list-refs
            ensure-storage-dirs))

(define *data-root* "./data")

(define (ensure-storage-dirs)
  "Ensure all storage directories exist"
  (for-each (lambda (dir)
              (let ((path (string-append *data-root* "/" dir)))
                (when (not (file-exists? path))
                  (system* "mkdir" "-p" path))))
            '("repos" "pushes" "objects" "refs")))

(define (store-push-data repo-name data)
  "Store raw push data for debugging"
  (ensure-storage-dirs)
  (let* ((timestamp (date->string (current-date) "~Y~m~d-~H~M~S"))
         (filename (format #f "~a/pushes/~a-~a.txt" 
                          *data-root* timestamp repo-name)))
    (call-with-output-file filename
      (lambda (port)
        (format port "=== Push to ~a at ~a ===~%" repo-name timestamp)
        (format port "~a~%" data)))
    (format #t "Stored push data in ~a~%" filename)
    filename))

(define (store-object sha content)
  "Store an object as plain text"
  (ensure-storage-dirs)
  (let ((filename (format #f "~a/objects/~a.txt" *data-root* sha)))
    (call-with-output-file filename
      (lambda (port)
        (if (string? content)
            (display content port)
            (put-bytevector port content))))
    (format #t "Stored object ~a~%" sha)
    sha))

(define (store-ref repo-name ref-name sha)
  "Store a reference as plain text"
  (ensure-storage-dirs)
  (let* ((repo-dir (format #f "~a/refs/~a" *data-root* repo-name))
         (_ (system* "mkdir" "-p" repo-dir))
         (filename (format #f "~a/~a.txt" 
                          repo-dir 
                          (string-map (lambda (c)
                                       (if (char=? c #\/) #\- c))
                                     ref-name))))
    (call-with-output-file filename
      (lambda (port)
        (format port "~a~%" sha)))
    (format #t "Stored ref ~a/~a -> ~a~%" repo-name ref-name sha)
    sha))

(define (read-ref repo-name ref-name)
  "Read a reference from plain text storage"
  (let ((filename (format #f "~a/refs/~a/~a.txt" 
                         *data-root* 
                         repo-name
                         (string-map (lambda (c)
                                      (if (char=? c #\/) #\- c))
                                    ref-name))))
    (if (file-exists? filename)
        (string-trim-both 
         (call-with-input-file filename get-string-all))
        #f)))

(define (list-refs repo-name)
  "List all refs for a repository"
  (let ((repo-dir (format #f "~a/refs/~a" *data-root* repo-name)))
    (if (file-exists? repo-dir)
        (let ((files (scandir repo-dir 
                             (lambda (f) 
                               (string-suffix? ".txt" f)))))
          (map (lambda (f)
                 (cons (substring f 0 (- (string-length f) 4))
                       (read-ref repo-name 
                                (substring f 0 (- (string-length f) 4)))))
               files))
        '())))