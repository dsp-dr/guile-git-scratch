(define-module (server receive-pack)
  #:use-module (server protocol)
  #:use-module (core repository)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:export (handle-receive-pack))

(define (handle-receive-pack input-port output-port repo-path)
  "Handle git-receive-pack protocol"
  (format (current-error-port) "receive-pack: Starting for ~a~%" repo-path)
  
  ;; Send reference advertisement
  (send-refs output-port repo-path "git-receive-pack")
  (force-output output-port)
  
  ;; Read commands from client
  (let ((commands (read-update-commands input-port)))
    (format (current-error-port) "receive-pack: Got ~a commands~%" 
            (length commands))
    
    (if (null? commands)
        ;; No commands, we're done
        (begin
          (format (current-error-port) "receive-pack: No commands received~%")
          #t)
        ;; Process pack data
        (begin
          ;; Read pack data
          (format (current-error-port) "receive-pack: Reading pack data~%")
          (let ((pack-received (parse-pack-data input-port repo-path)))
            
            ;; Update references
            (for-each
             (lambda (cmd)
               (let ((old-sha (list-ref cmd 0))
                     (new-sha (list-ref cmd 1))
                     (ref-name (list-ref cmd 2)))
                 (format (current-error-port) 
                         "receive-pack: Updating ~a from ~a to ~a~%"
                         ref-name old-sha new-sha)
                 ;; Update the reference
                 (if (string=? new-sha (make-string 40 #\0))
                     ;; Delete ref
                     (delete-ref repo-path ref-name)
                     ;; Update ref
                     (update-ref repo-path ref-name new-sha))))
             commands)
            
            ;; Send report
            (send-report output-port commands)
            (force-output output-port)
            #t)))))

(define (read-update-commands port)
  "Read update commands from client"
  (let loop ((commands '()))
    (let ((line (pkt-line-read port)))
      (cond
       ((or (not line) (eq? line 'flush))
        (reverse commands))
       ((string? line)
        (let ((parts (string-split line #\space)))
          (if (= (length parts) 3)
              (loop (cons parts commands))
              (loop commands))))
       (else (loop commands))))))

(define (delete-ref repo-path ref-name)
  "Delete a reference"
  (let ((ref-path (string-append repo-path "/" ref-name)))
    (when (file-exists? ref-path)
      (delete-file ref-path))))

(define (send-report port commands)
  "Send status report to client"
  ;; Send unpack status
  (pkt-line-write port "unpack ok")
  
  ;; Send command status
  (for-each
   (lambda (cmd)
     (let ((ref-name (list-ref cmd 2)))
       (pkt-line-write port (format #f "ok ~a" ref-name))))
   commands)
  
  (pkt-flush port))