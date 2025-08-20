(define-module (core zlib-simple)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 popen)
  #:export (compress-bytevector
            decompress-bytevector))

;; Simple zlib implementation using shell commands
;; This is temporary until we get proper FFI working

(define* (compress-bytevector bv #:key (level 6))
  "Compress a bytevector using gzip command"
  (let* ((port (open-pipe* OPEN_BOTH "gzip" "-c" (format #f "-~d" level)))
         (_ (put-bytevector port bv))
         (_ (close-output-port port))
         (result (get-bytevector-all port)))
    (close-pipe port)
    result))

(define (decompress-bytevector bv)
  "Decompress a bytevector using gunzip command"
  (let* ((port (open-pipe* OPEN_BOTH "gunzip" "-c"))
         (_ (put-bytevector port bv))
         (_ (close-output-port port))
         (result (get-bytevector-all port)))
    (close-pipe port)
    result))