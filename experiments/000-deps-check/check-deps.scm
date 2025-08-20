#!/usr/bin/env guile3
!#

(use-modules (ice-9 format))

(define (check-command cmd)
  (zero? (system (string-append cmd " > /dev/null 2>&1"))))

(define (check-guile-feature feature)
  (catch #t
    (lambda ()
      (eval `(use-modules ,feature) (current-module))
      #t)
    (lambda _ #f)))

(format #t "=== FreeBSD Git Implementation Environment Check ===~%~%")

(format #t "Operating System:~%")
(system "uname -a")
(newline)

(format #t "Guile Version:~%")
(system "guile3 --version | head -1")
(newline)

(format #t "GNU Make:~%")
(system "gmake --version | head -1")
(newline)

(format #t "Checking Guile modules:~%")
(define modules-to-check
  '((ice-9 match)
    (ice-9 binary-ports)
    (ice-9 textual-ports)
    (ice-9 format)
    (ice-9 regex)
    (ice-9 ftw)
    (srfi srfi-1)
    (srfi srfi-9)
    (srfi srfi-19)
    (srfi srfi-26)
    (srfi srfi-64)
    (rnrs bytevectors)
    (system foreign)))

(for-each
 (lambda (module)
   (format #t "  ~a: ~a~%"
           module
           (if (check-guile-feature module) "✓" "✗")))
 modules-to-check)

(newline)
(format #t "Checking system libraries:~%")

(format #t "  zlib: ")
(if (check-command "pkg info -e zlib")
    (format #t "✓~%")
    (format #t "✗ (needed for compression)~%"))

(format #t "  openssl: ")
(if (check-command "pkg info -e openssl")
    (format #t "✓~%")
    (format #t "✗ (needed for SHA-1)~%"))

(newline)
(format #t "Checking for Git (reference implementation):~%")
(system "git --version")

(newline)
(format #t "=== Check Complete ===~%")