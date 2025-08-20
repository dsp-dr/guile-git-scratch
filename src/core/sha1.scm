(define-module (core sha1)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:export (sha1-bytevector
            sha1-string
            bytevector->hex-string
            hex-string->bytevector))

;; FFI bindings to libcrypto (OpenSSL) for SHA-1
;; FreeBSD should have this in the base system

(define libcrypto (dynamic-link "libcrypto.so"))

;; SHA1 context size (from openssl/sha.h)
(define SHA_DIGEST_LENGTH 20)
(define SHA_CTX_SIZE 96) ; Approximate, platform-dependent

;; FFI function bindings
(define SHA1_Init
  (pointer->procedure int
                     (dynamic-func "SHA1_Init" libcrypto)
                     (list '*)))

(define SHA1_Update
  (pointer->procedure int
                     (dynamic-func "SHA1_Update" libcrypto)
                     (list '* '* size_t)))

(define SHA1_Final
  (pointer->procedure int
                     (dynamic-func "SHA1_Final" libcrypto)
                     (list '* '*)))

(define (sha1-bytevector bv)
  "Compute SHA-1 hash of a bytevector, return as bytevector"
  (let* ((ctx (make-bytevector SHA_CTX_SIZE 0))
         (ctx-ptr (bytevector->pointer ctx))
         (hash (make-bytevector SHA_DIGEST_LENGTH 0))
         (hash-ptr (bytevector->pointer hash))
         (data-ptr (bytevector->pointer bv))
         (len (bytevector-length bv)))
    (SHA1_Init ctx-ptr)
    (SHA1_Update ctx-ptr data-ptr len)
    (SHA1_Final hash-ptr ctx-ptr)
    hash))

(define (sha1-string str)
  "Compute SHA-1 hash of a string, return as bytevector"
  (sha1-bytevector (string->utf8 str)))

(define (bytevector->hex-string bv)
  "Convert bytevector to hex string"
  (string-concatenate
   (map (lambda (byte)
          (format #f "~2,'0x" byte))
        (bytevector->u8-list bv))))

(define (hex-string->bytevector hex)
  "Convert hex string to bytevector"
  (let* ((len (/ (string-length hex) 2))
         (bv (make-bytevector len)))
    (let loop ((i 0))
      (when (< i len)
        (bytevector-u8-set! bv i
                           (string->number 
                            (substring hex (* i 2) (+ (* i 2) 2))
                            16))
        (loop (+ i 1))))
    bv))