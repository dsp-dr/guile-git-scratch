(define-module (core sha1-simple)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (sha1-bytevector
            sha1-string
            bytevector->hex-string
            hex-string->bytevector))

;; Simple SHA-1 implementation using shell command
;; This is temporary until we get proper FFI or guile-gcrypt working

(define (sha1-bytevector bv)
  "Compute SHA-1 hash of a bytevector using openssl command"
  (let* ((port (open-pipe* OPEN_BOTH "openssl" "dgst" "-sha1" "-binary"))
         (_ (put-bytevector port bv))
         (_ (close-output-port port))
         (result (get-bytevector-all port)))
    (close-pipe port)
    result))

(define (sha1-string str)
  "Compute SHA-1 hash of a string"
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