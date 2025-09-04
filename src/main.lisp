(in-package :tle)

(defun run-webview (&key title url width height)
  "Launch a webview window pointing to the given URL."
  (float-features:with-float-traps-masked t
    (let ((w (webview:webview-create 0 (cffi:null-pointer))))
      (unwind-protect
           (progn
             (webview:webview-set-title w title)
             (webview:webview-set-size w width height 0)
             (webview:webview-navigate w url)
             (webview:webview-run w))
        (webview:webview-destroy w)))))

(defun find-available-port (&optional (start-port 8080))
  "Find an available port starting from the given port."
  (handler-case
      (let ((test-socket (usocket:socket-listen "127.0.0.1" start-port :reuse-address t)))
        (usocket:socket-close test-socket)
        start-port)
    (usocket:address-in-use-error ()
      (find-available-port (1+ start-port)))))

(defun main ()
  (let ((ui (make-web-ui))
        (editor (make-standard-editor))
        (port (let ((env-port (uiop:getenv "TLE_PORT")))
                (if env-port
                    (parse-integer env-port :junk-allowed t)
                    (find-available-port)))))
    
    ;; Start the server in a background thread
    (bt:make-thread 
     (lambda ()
       (run-web-ui ui editor port))
     :name "tle-server")
    
    ;; Give the server a moment to start
    (sleep 1)
    
    ;; Launch webview pointing to the server
    (run-webview :title "TLE - Timmy's Lisp Environment"
                 :url (format nil "http://127.0.0.1:~D" port)
                 :width 1200
                 :height 800)
    
    ;; Exit when webview closes
    (uiop:quit)))