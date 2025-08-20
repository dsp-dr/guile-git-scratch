(define-module (core zlib)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (compress-bytevector
            decompress-bytevector))

;; FFI bindings to zlib
(define libz (dynamic-link "libz.so"))

;; zlib compress2 function
(define compress2
  (pointer->procedure int
                     (dynamic-func "compress2" libz)
                     (list '* '* '* int)))

;; zlib uncompress function
(define uncompress
  (pointer->procedure int
                     (dynamic-func "uncompress" libz)
                     (list '* '* '* size_t)))

(define Z_BEST_SPEED 1)
(define Z_BEST_COMPRESSION 9)
(define Z_DEFAULT_COMPRESSION 6)

(define* (compress-bytevector bv #:key (level Z_DEFAULT_COMPRESSION))
  "Compress a bytevector using zlib"
  (let* ((src-len (bytevector-length bv))
         ;; Allocate output buffer (worst case is slightly larger than input)
         (dest-len-max (+ src-len (quotient src-len 1000) 12))
         (dest (make-bytevector dest-len-max))
         (dest-len-bv (make-bytevector (sizeof size_t)))
         (src-ptr (bytevector->pointer bv))
         (dest-ptr (bytevector->pointer dest))
         (dest-len-ptr (bytevector->pointer dest-len-bv)))
    
    ;; Set initial destination length
    (bytevector-uint-set! dest-len-bv 0 dest-len-max 
                          (native-endianness) (sizeof size_t))
    
    ;; Compress
    (let ((result (compress2 dest-ptr dest-len-ptr src-ptr src-len level)))
      (if (zero? result)
          ;; Return compressed data trimmed to actual size
          (let ((actual-len (bytevector-uint-ref dest-len-bv 0 
                                                 (native-endianness) 
                                                 (sizeof size_t))))
            (bytevector-copy dest 0 actual-len))
          (error "Compression failed" result)))))

(define (decompress-bytevector bv expected-size)
  "Decompress a bytevector using zlib. Need to know expected output size."
  (let* ((dest (make-bytevector expected-size))
         (dest-len-bv (make-bytevector (sizeof size_t)))
         (src-ptr (bytevector->pointer bv))
         (dest-ptr (bytevector->pointer dest))
         (dest-len-ptr (bytevector->pointer dest-len-bv)))
    
    ;; Set expected destination length
    (bytevector-uint-set! dest-len-bv 0 expected-size
                          (native-endianness) (sizeof size_t))
    
    ;; Decompress
    (let ((result (uncompress dest-ptr dest-len-ptr 
                             src-ptr (bytevector-length bv))))
      (if (zero? result)
          dest
          (error "Decompression failed" result)))))