#!/usr/bin/env guile3
!#

(use-modules (ice-9 format)
             (rnrs bytevectors)
             (ice-9 iconv))

;; Use Guile's built-in gcrypt bindings for SHA-1
(use-modules (gcrypt hash))

(define (sha1-string str)
  "Compute SHA-1 hash of a string, return as hex string"
  (let* ((bv (string->bytevector str "UTF-8"))
         (hash (sha1 bv)))
    (bytevector->hex-string hash)))

(define (sha1-bytevector bv)
  "Compute SHA-1 hash of a bytevector, return as hex string"
  (bytevector->hex-string (sha1 bv)))

(define (bytevector->hex-string bv)
  "Convert bytevector to hex string"
  (string-concatenate
   (map (lambda (byte)
          (format #f "~2,'0x" byte))
        (bytevector->u8-list bv))))

;; Test cases
(format #t "=== SHA-1 Hashing Tests ===~%~%")

(define test-string "blob 11\x00Hello, Git!")
(format #t "Input: ~s~%" test-string)
(format #t "SHA-1: ~a~%~%" (sha1-string test-string))

(define test-string2 "The quick brown fox jumps over the lazy dog")
(format #t "Input: ~s~%" test-string2)
(format #t "SHA-1: ~a~%" (sha1-string test-string2))
(format #t "Expected: 2fd4e1c67a2d28fced849ee1bb76e7391b93eb12~%~%")

;; Git object hash test
(define (git-object-hash type content)
  "Compute Git object hash"
  (let* ((size (bytevector-length content))
         (header (string->bytevector (format #f "~a ~d\x00" type size) "UTF-8"))
         (full-content (bytevector-append header content)))
    (sha1-bytevector full-content)))

(define blob-content (string->bytevector "Hello, World!\n" "UTF-8"))
(format #t "Git blob hash for \"Hello, World!\\n\":~%")
(format #t "  ~a~%" (git-object-hash "blob" blob-content))
(format #t "  Expected: 8ab686eafeb1f44702738c8b0f24f2567c36da6d~%")