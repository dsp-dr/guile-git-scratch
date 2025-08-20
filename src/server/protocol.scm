(define-module (server protocol)
  #:use-module (core objects)
  #:use-module (core repository)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:export (pkt-line-write
            pkt-line-read
            pkt-flush
            send-refs
            parse-want-lines
            parse-pack-data))

;; Git protocol packet line format
(define (pkt-line-write port data)
  "Write data in pkt-line format"
  (if (string? data)
      (let* ((line (string-append data "\n"))
             (len (+ 4 (string-length line)))
             (hex-len (format #f "~4,'0x" len)))
        (display hex-len port)
        (display line port))
      ;; For binary data
      (let* ((len (+ 4 (bytevector-length data)))
             (hex-len (format #f "~4,'0x" len)))
        (display hex-len port)
        (put-bytevector port data))))

(define (pkt-line-read port)
  "Read a pkt-line from port"
  (let ((len-str (get-string-n port 4)))
    (if (eof-object? len-str)
        #f
        (let ((len (string->number len-str 16)))
          (cond
           ((= len 0) 'flush)  ; flush packet
           ((= len 1) 'delim)  ; delimiter packet  
           ((= len 2) 'response-end) ; response end
           (else
            (let ((data (get-string-n port (- len 4))))
              (string-trim-right data #\newline))))))))

(define (pkt-flush port)
  "Send a flush packet"
  (display "0000" port)
  (force-output port))

(define (send-refs port repo-path service)
  "Send reference advertisement"
  ;; Send service announcement
  (pkt-line-write port (format #f "# service=~a" service))
  (pkt-flush port)
  
  ;; Send capabilities with first ref
  (let ((refs (get-all-refs repo-path)))
    (if (null? refs)
        ;; No refs yet - send capabilities with zero ID
        (begin
          (pkt-line-write port 
            (string-append (make-string 40 #\0) 
                          " capabilities^{}\x00"
                          "report-status delete-refs side-band-64k quiet "
                          "ofs-delta agent=guile-git/0.1"))
          (pkt-flush port))
        ;; Send refs with capabilities on first one
        (let loop ((refs refs) (first #t))
          (when (not (null? refs))
            (let* ((ref (car refs))
                   (sha (car ref))
                   (name (cdr ref)))
              (if first
                  (pkt-line-write port
                    (format #f "~a ~a\x00report-status side-band-64k"
                           sha name))
                  (pkt-line-write port
                    (format #f "~a ~a" sha name)))
              (loop (cdr refs) #f))))
        (pkt-flush port))))

(define (get-all-refs repo-path)
  "Get all references in the repository"
  (let ((refs '()))
    ;; Check HEAD
    (let ((head (read-ref repo-path "HEAD")))
      (when head
        (set! refs (cons (cons head "HEAD") refs))))
    ;; Check branches
    (let ((heads-dir (string-append repo-path "/refs/heads")))
      (when (file-exists? heads-dir)
        (for-each 
         (lambda (branch)
           (when (not (member branch '("." "..")))
             (let* ((ref-path (string-append "refs/heads/" branch))
                    (sha (read-ref repo-path ref-path)))
               (when sha
                 (set! refs (cons (cons sha ref-path) refs))))))
         (scandir heads-dir))))
    ;; Check tags
    (let ((tags-dir (string-append repo-path "/refs/tags")))
      (when (file-exists? tags-dir)
        (for-each
         (lambda (tag)
           (when (not (member tag '("." "..")))
             (let* ((ref-path (string-append "refs/tags/" tag))
                    (sha (read-ref repo-path ref-path)))
               (when sha
                 (set! refs (cons (cons sha ref-path) refs))))))
         (scandir tags-dir))))
    (reverse refs)))

(define (parse-want-lines port)
  "Parse want lines from client"
  (let loop ((wants '()))
    (let ((line (pkt-line-read port)))
      (cond
       ((eq? line 'flush) (reverse wants))
       ((string-prefix? "want " line)
        (loop (cons (substring line 5 45) wants)))
       ((string-prefix? "have " line)
        (loop wants)) ; Ignore have lines for now
       (else (loop wants))))))

(define (parse-pack-data port repo-path)
  "Parse and store pack data from client"
  ;; For now, just read and store raw pack data
  ;; Full pack parsing would be complex
  (let ((pack-data (get-bytevector-all port)))
    (when (and pack-data (> (bytevector-length pack-data) 0))
      ;; Store pack temporarily
      (let ((pack-file (string-append repo-path "/objects/pack/tmp.pack")))
        (system* "mkdir" "-p" (string-append repo-path "/objects/pack"))
        (call-with-output-file pack-file
          (lambda (out)
            (put-bytevector out pack-data)))
        ;; In a real implementation, we'd unpack objects here
        #t))))