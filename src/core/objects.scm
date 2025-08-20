(define-module (core objects)
  #:use-module (core sha1-simple)
  #:use-module (core zlib-simple)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (hash-object
            write-object
            read-object
            object-path
            ensure-git-dir))

(define (ensure-git-dir repo-path)
  "Ensure .git directory structure exists"
  (for-each (lambda (dir)
              (let ((path (string-append repo-path "/" dir)))
                (when (not (file-exists? path))
                  (mkdir-p path))))
            '(".git" ".git/objects" ".git/refs" ".git/refs/heads")))

(define (mkdir-p path)
  "Create directory and parents if needed"
  (system* "mkdir" "-p" path))

(define (object-path repo-path sha)
  "Get the path for an object given its SHA-1"
  (string-append repo-path "/.git/objects/"
                (substring sha 0 2) "/"
                (substring sha 2)))

(define (hash-object type content)
  "Compute the SHA-1 hash for a Git object"
  (let* ((size (bytevector-length content))
         (header (string->utf8 (format #f "~a ~d\x00" type size)))
         (full-content (bytevector-append header content)))
    (bytevector->hex-string (sha1-bytevector full-content))))

(define (write-object repo-path type content)
  "Write an object to the repository and return its SHA-1"
  (let* ((size (bytevector-length content))
         (header (string->utf8 (format #f "~a ~d\x00" type size)))
         (full-content (bytevector-append header content))
         (sha (bytevector->hex-string (sha1-bytevector full-content)))
         (compressed (compress-bytevector full-content))
         (obj-dir (string-append repo-path "/.git/objects/" (substring sha 0 2)))
         (obj-path (string-append obj-dir "/" (substring sha 2))))
    
    ;; Create object directory if needed
    (when (not (file-exists? obj-dir))
      (mkdir-p obj-dir))
    
    ;; Write compressed object
    (call-with-output-file obj-path
      (lambda (port)
        (put-bytevector port compressed)))
    
    sha))

(define (read-object repo-path sha)
  "Read an object from the repository"
  (let* ((obj-path (object-path repo-path sha)))
    (if (file-exists? obj-path)
        (let* ((compressed (call-with-input-file obj-path
                            (lambda (port)
                              (get-bytevector-all port))))
               ;; We need to guess the uncompressed size
               ;; Git objects are typically small, start with 64KB
               (decompressed (decompress-with-unknown-size compressed)))
          (parse-object decompressed))
        (error "Object not found" sha))))

(define (decompress-with-unknown-size compressed)
  "Decompress when we don't know the output size"
  ;; Try progressively larger sizes
  (let loop ((size 65536)) ; Start with 64KB
    (catch #t
      (lambda ()
        (decompress-bytevector compressed size))
      (lambda _
        (if (< size (* 16 1024 1024)) ; Max 16MB
            (loop (* size 2))
            (error "Object too large or corrupt"))))))

(define (parse-object data)
  "Parse a Git object from decompressed data"
  (let* ((null-pos (bytevector-index data 0))
         (header (utf8->string (bytevector-copy data 0 null-pos)))
         (content (bytevector-copy data (+ null-pos 1)))
         (parts (string-split header #\space)))
    (list (string->symbol (car parts))
          (string->number (cadr parts))
          content)))

(define (bytevector-index bv byte)
  "Find index of byte in bytevector"
  (let loop ((i 0))
    (cond ((>= i (bytevector-length bv)) #f)
          ((= (bytevector-u8-ref bv i) byte) i)
          (else (loop (+ i 1))))))