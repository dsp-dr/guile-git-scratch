#!/usr/bin/env guile3
!#

(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (srfi srfi-64)
             (core sha1-simple)
             (rnrs bytevectors))

(test-begin "sha1-tests")

(test-group "SHA-1 hashing"
  (test-equal "Empty string SHA-1"
    "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    (bytevector->hex-string (sha1-string "")))
  
  (test-equal "Hello World SHA-1"
    "0a4d55a8d778e5022fab701977c5d840bbc486d0"
    (bytevector->hex-string (sha1-string "Hello World")))
  
  (test-equal "Git blob object format"
    (string-length "8ab686eafeb1f44702738c8b0f24f2567c36da6d")
    (string-length (bytevector->hex-string 
                    (sha1-string "blob 14\x00Hello, World!\n")))))

(test-group "Hex conversions"
  (test-equal "Bytevector to hex"
    "48656c6c6f"
    (bytevector->hex-string (string->utf8 "Hello")))
  
  (test-equal "Hex to bytevector"
    #vu8(72 101 108 108 111)
    (hex-string->bytevector "48656c6c6f")))

(test-end "sha1-tests")