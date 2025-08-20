(define-module (server server)
  #:use-module (server receive-pack)
  #:use-module (server protocol)
  #:use-module (core repository)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 threads)
  #:use-module ((ice-9 binary-ports) #:select (get-bytevector-n put-bytevector))
  #:export (start-git-server))

(define* (start-git-server #:key (port 9418) (repos-path "./repos"))
  "Start a minimal Git server"
  (format #t "Starting Guile Git server on port ~a~%" port)
  (format #t "Repository path: ~a~%" repos-path)
  
  ;; Create repos directory if it doesn't exist
  (when (not (file-exists? repos-path))
    (system* "mkdir" "-p" repos-path))
  
  ;; Create TCP server socket
  (let ((server-socket (socket PF_INET SOCK_STREAM 0)))
    ;; Allow reuse of address
    (setsockopt server-socket SOL_SOCKET SO_REUSEADDR 1)
    
    ;; Bind to port
    (bind server-socket (make-socket-address AF_INET INADDR_ANY port))
    
    ;; Listen for connections
    (listen server-socket 5)
    
    (format #t "Server listening on port ~a...~%" port)
    (format #t "To test: git remote add local-test git://localhost:~a/test.git~%~%" port)
    
    ;; Accept connections
    (let loop ()
      (let* ((client-connection (accept server-socket))
             (client-socket (car client-connection))
             (client-addr (cdr client-connection)))
        
        (format #t "Connection from ~a~%" client-addr)
        
        ;; Handle in new thread
        (call-with-new-thread
         (lambda ()
           (handle-client client-socket repos-path)))
        
        (loop)))))

(define (handle-client socket repos-path)
  "Handle a client connection"
  (catch #t
    (lambda ()
      ;; Read request line
      (let* ((line (read-line socket))
             (parts (string-split line #\space)))
        
        (format #t "Request: ~a~%" line)
        
        (when (>= (length parts) 2)
          (let* ((command (car parts))
                 (path (cadr parts))
                 ;; Extract repository name from path
                 (repo-match (string-match "^/?([^/]+\\.git)" path))
                 (repo-name (if repo-match
                               (match:substring repo-match 1)
                               "test.git"))
                 (repo-path (string-append repos-path "/" repo-name)))
            
            (format #t "Command: ~a, Repository: ~a~%" command repo-name)
            
            ;; Ensure repository exists
            (when (not (file-exists? repo-path))
              (format #t "Creating repository: ~a~%" repo-path)
              (init-repository repo-path #:bare #t))
            
            ;; Handle git commands
            (cond
             ((string=? command "git-receive-pack")
              (handle-receive-pack socket socket repo-path))
             ((string=? command "git-upload-pack")
              (handle-upload-pack socket socket repo-path))
             (else
              (format #t "Unknown command: ~a~%" command)))))))
    
    (lambda (key . args)
      (format (current-error-port) "Error handling client: ~a ~a~%" key args)))
  
  (close-port socket))

(define (handle-upload-pack input-port output-port repo-path)
  "Handle git-upload-pack protocol (fetch/clone)"
  (format (current-error-port) "upload-pack: Starting for ~a~%" repo-path)
  
  ;; Send reference advertisement
  (send-refs output-port repo-path "git-upload-pack")
  (force-output output-port)
  
  ;; For now, just read wants and send empty pack
  (let ((wants (parse-want-lines input-port)))
    (format (current-error-port) "upload-pack: Client wants ~a objects~%" 
            (length wants))
    
    ;; Send NAK
    (pkt-line-write output-port "NAK")
    (pkt-flush output-port)
    
    ;; For minimal implementation, send empty pack
    ;; Real implementation would send requested objects
    (format (current-error-port) "upload-pack: Sending empty pack~%")
    (force-output output-port)))