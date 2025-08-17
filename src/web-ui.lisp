(in-package :tle)

(defclass web-ui (ui-implementation)
  ((name :initform "Web" :reader name)
   (server-socket :initform nil :accessor server-socket)
   (client-connections :initform nil :accessor client-connections))
  (:documentation "A UI implementation using a basic webserver."))

(defvar *editor-instance* nil)
(defvar *web-ui-instance* nil)
(defvar *server-running* nil)

(defun get-buffer-text (buffer)
  "Constructs a single string from the buffer's lines."
  (let ((lines (loop for i from 0 below (buffer-line-count buffer)
                     collect (buffer-line buffer i))))
    (format nil "~{~A~^~%~}" lines)))

(defparameter *html-template*
  "<!DOCTYPE html>
<html>
<head>
    <title>TLE</title>
    <style>
        body { 
            background-color: #282c34; 
            color: #abb2bf; 
            font-family: monospace; 
            margin: 0; 
            padding: 0; 
            overflow: hidden; 
            position: relative;
        }
        .window {
            position: absolute;
            background-color: #3e4451;
            border: 1px solid #5c6370;
            border-radius: 4px;
            min-width: 200px;
            min-height: 100px;
            box-shadow: 0 4px 8px rgba(0,0,0,0.3);
        }
        .window-header {
            background-color: #4b5263;
            padding: 8px 12px;
            cursor: move;
            border-bottom: 1px solid #5c6370;
            user-select: none;
            font-weight: bold;
        }
        .window-content {
            padding: 10px;
            white-space: pre;
            font-size: 16px;
            line-height: 1.4;
            overflow: auto;
            height: calc(100% - 40px);
        }
        .window-resizer {
            position: absolute;
            bottom: 0;
            right: 0;
            width: 10px;
            height: 10px;
            cursor: se-resize;
            background: linear-gradient(-45deg, transparent 30%, #5c6370 30%, #5c6370 70%, transparent 70%);
        }
        #editor { 
            white-space: pre; 
            font-size: 16px; 
            line-height: 1.4; 
            padding: 5px; 
        }
    </style>
</head>
<body>
    <div id=\"app-container\">~A</div>
    <script>
        let eventSource = null;
        let draggedWindow = null;
        let dragOffset = {x: 0, y: 0};
        let resizedWindow = null;
        
        function makeWindowDraggable(windowElement) {
            const header = windowElement.querySelector('.window-header');
            const resizer = windowElement.querySelector('.window-resizer');
            
            header.addEventListener('mousedown', startDrag);
            resizer.addEventListener('mousedown', startResize);
        }
        
        function startDrag(e) {
            draggedWindow = e.target.closest('.window');
            const rect = draggedWindow.getBoundingClientRect();
            dragOffset.x = e.clientX - rect.left;
            dragOffset.y = e.clientY - rect.top;
            
            document.addEventListener('mousemove', drag);
            document.addEventListener('mouseup', stopDrag);
            e.preventDefault();
        }
        
        function drag(e) {
            if (!draggedWindow) return;
            
            const x = e.clientX - dragOffset.x;
            const y = e.clientY - dragOffset.y;
            
            draggedWindow.style.left = Math.max(0, x) + 'px';
            draggedWindow.style.top = Math.max(0, y) + 'px';
        }
        
        function stopDrag() {
            draggedWindow = null;
            document.removeEventListener('mousemove', drag);
            document.removeEventListener('mouseup', stopDrag);
        }
        
        function startResize(e) {
            resizedWindow = e.target.closest('.window');
            document.addEventListener('mousemove', resize);
            document.addEventListener('mouseup', stopResize);
            e.preventDefault();
            e.stopPropagation();
        }
        
        function resize(e) {
            if (!resizedWindow) return;
            
            const rect = resizedWindow.getBoundingClientRect();
            const width = e.clientX - rect.left;
            const height = e.clientY - rect.top;
            
            resizedWindow.style.width = Math.max(200, width) + 'px';
            resizedWindow.style.height = Math.max(100, height) + 'px';
        }
        
        function stopResize() {
            resizedWindow = null;
            document.removeEventListener('mousemove', resize);
            document.removeEventListener('mouseup', stopResize);
        }
        
        function initializeWindows() {
            document.querySelectorAll('.window').forEach(makeWindowDraggable);
        }
        
        function connectEventSource() {
            eventSource = new EventSource('/events');
            
            eventSource.onopen = function() {
                console.log('EventSource connected');
                requestUpdate();
            };
            
            eventSource.onmessage = function(event) {
                const data = JSON.parse(event.data);
                if (data.type === 'update') {
                    document.getElementById('app-container').innerHTML = data.content;
                    initializeWindows();
                }
            };
            
            eventSource.onerror = function() {
                console.log('EventSource error');
                eventSource.close();
                setTimeout(connectEventSource, 1000);
            };
        }
        
        function requestUpdate() {
            fetch('/update', { method: 'POST' });
        }
        
        function sendKey(keyInfo) {
            fetch('/key', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify(keyInfo)
            });
        }
        
        document.addEventListener('keydown', (e) => {
            e.preventDefault();
            let key = e.key;
            if (key === 'Dead') return;
            let keyInfo = { 
                key: e.key, 
                code: e.code, 
                ctrl: e.ctrlKey, 
                alt: e.altKey, 
                shift: e.shiftKey, 
                meta: e.metaKey 
            };
            sendKey(keyInfo);
        });
        
        connectEventSource();
        initializeWindows();
    </script>
</body>
</html>")

(defun escape-html (text)
  "Escape HTML special characters in text."
  (let ((escaped text))
    (setf escaped (substitute-string escaped "&" "&amp;"))
    (setf escaped (substitute-string escaped "<" "&lt;"))
    (setf escaped (substitute-string escaped ">" "&gt;"))
    escaped))

(defmethod render-application ((app application))
  "Render an application as HTML with all its frames as draggable windows."
  (let ((frames (application-frames app))
        (editor (application-editor app)))
    (if (null frames)
        ;; If no frames, create a default window with the current buffer
        (let* ((buffer (when editor (current-buffer editor)))
               (content (if buffer (escape-html (get-buffer-text buffer)) "No content")))
          (format nil "<div class=\"window\" style=\"left: 50px; top: 50px; width: 600px; height: 400px;\">
  <div class=\"window-header\">~A - Buffer</div>
  <div class=\"window-content\">~A</div>
  <div class=\"window-resizer\"></div>
</div>" (application-name app) content))
        ;; Render all frames as windows
        (let ((window-html "")
              (x-offset 50)
              (y-offset 50))
          (dolist (frame frames)
            (let ((frame-content (escape-html (format nil "Frame: ~A" frame))))
              (setf window-html
                    (concatenate 'string window-html
                                 (format nil "<div class=\"window\" style=\"left: ~Apx; top: ~Apx; width: 400px; height: 300px;\">
  <div class=\"window-header\">Frame ~A</div>
  <div class=\"window-content\">~A</div>
  <div class=\"window-resizer\"></div>
</div>" x-offset y-offset frame frame-content)))
              (incf x-offset 30)
              (incf y-offset 30)))
          window-html))))

(defun substitute-string (string old new)
  "Simple string substitution function."
  (let ((pos (search old string)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (substitute-string (subseq string (+ pos (length old))) old new))
        string)))

(defun split-string (string delimiter)
  "Split string by delimiter."
  (let ((result '())
        (start 0))
    (loop for pos = (position (char delimiter 0) string :start start)
          do (push (subseq string start pos) result)
             (if pos
                 (setf start (1+ pos))
                 (return)))
    (nreverse result)))

(defun read-http-body (client-stream content-length)
  "Read HTTP request body."
  (when (and content-length (> content-length 0))
    (let ((body (make-string content-length)))
      (read-sequence body client-stream)
      body)))

(defun parse-http-headers (client-stream)
  "Parse HTTP headers and return as alist."
  (handler-case
      (let ((headers '()))
        (loop for line = (read-line client-stream nil nil)
              while line
              do (progn
                   (format t "Read header line: '~A'~%" line)
                   (when (or (string= line "") (string= line (string #\Return)))
                     ;;(format t "Found empty line, ending header parsing~%")
                     (return))
                   (let ((colon-pos (position #\: line)))
                     (when colon-pos
                       (push (cons (string-trim " " (subseq line 0 colon-pos))
                                   (string-trim " " (subseq line (1+ colon-pos))))
                             headers)))))
        (setf headers (nreverse headers))
        (format t "Headers read: ~A~%" headers)
        headers)
    (error (e)
      (format t "Error parsing headers: ~A~%" e)
      nil)))

(defun parse-query-parameters (path)
  "Parse query parameters from a URL path."
  (let ((query-pos (position #\? path)))
    (if query-pos
        (let ((query-string (subseq path (1+ query-pos)))
              (path-only (subseq path 0 query-pos))
              (params (make-hash-table :test 'equal)))
          (dolist (param (split-string query-string "&"))
            (let ((equals-pos (position #\= param)))
              (when equals-pos
                (setf (gethash (subseq param 0 equals-pos) params)
                      (subseq param (1+ equals-pos))))))
          (values path-only params))
        (values path (make-hash-table :test 'equal)))))


(defun handle-http-request (client-stream)
  "Handle HTTP request and send appropriate response. Returns :keep-alive or :close."
  (handler-case
      (let ((request-line (read-line client-stream nil nil)))
        (format t "Request line: ~A~%" request-line)
        (if request-line
            (let* ((parts (split-string request-line " "))
                   (method (first parts))
                   (full-path (second parts))
                   (headers (parse-http-headers client-stream))
                   (content-length (parse-integer (or (cdr (assoc "Content-Length" headers :test #'string=)) "0") :junk-allowed t))
                   (body (read-http-body client-stream content-length)))
              (multiple-value-bind (path query-params) (parse-query-parameters full-path)
                (let ((app-name (or (gethash "app" query-params) *default-application-name*)))
                  (format t "Method: ~A, Path: ~A, App: ~A~%" method path app-name)
                  
                  (cond
                    ((and (string= method "GET") (string= path "/"))
                     (send-html-response client-stream app-name)
                     :close)
                    ((and (string= method "GET") (string= path "/events"))
                     (send-sse-response client-stream app-name)
                     :keep-alive)
                    ((and (string= method "POST") (string= path "/key"))
                     (handle-key-post client-stream body app-name)
                     :close)
                    ((and (string= method "POST") (string= path "/update"))
                     (handle-update-post client-stream app-name)
                     :close)
                    (t
                     (send-404-response client-stream)
                     :close)))))
            :close))
    (error (e)
      (format t "Error handling request: ~A~%" e)
      (ignore-errors (send-404-response client-stream))
      :close)))

(defun send-html-response (client-stream &optional (app-name *default-application-name*))
  "Send HTML page response."
  (format t "Sending HTML response for app: ~A~%" app-name)
  (let* ((app (get-application app-name))
         (rendered-content (if app (render-application app) "<div id=\"editor\">No application found</div>"))
         (html-content (format nil *html-template* rendered-content))
         (content-length (length html-content))
         (response (format nil "HTTP/1.1 200 OK~C~AContent-Type: text/html~C~AContent-Length: ~A~C~AConnection: close~C~A~C~A~A"
                          #\Return #\Linefeed #\Return #\Linefeed content-length #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed html-content)))
    (write-string response client-stream)
    (force-output client-stream)
    (finish-output client-stream)
    (format t "HTML response sent~%")))

(defun send-sse-response (client-stream &optional (app-name *default-application-name*))
  "Send Server-Sent Events response."
  (push client-stream (client-connections *web-ui-instance*))
  (format client-stream "HTTP/1.1 200 OK~%")
  (format client-stream "Content-Type: text/event-stream~%")
  (format client-stream "Cache-Control: no-cache~%")
  (format client-stream "Connection: keep-alive~%")
  (format client-stream "~%")
  (force-output client-stream)
  
  ;; Send initial content
  (send-content-update client-stream app-name)
  
  ;; Keep connection alive in background
  (bt:make-thread
   (lambda ()
     (loop
       (sleep 30)
       (ignore-errors
         (format client-stream "data: ~A~%~%" 
                 (jsown:to-json (jsown:new-js ("type" "ping"))))
         (force-output client-stream))))
   :name "sse-keepalive"))

(defun send-404-response (client-stream)
  "Send 404 Not Found response."
  (format client-stream "HTTP/1.1 404 Not Found~%")
  (format client-stream "Content-Length: 0~%")
  (format client-stream "~%")
  (force-output client-stream))

(defun handle-key-post (client-stream body &optional (app-name *default-application-name*))
  "Handle key input POST request."
  (when body
    (let* ((data (jsown:parse body)))
      (handle-key-input data app-name)))
  
  (format client-stream "HTTP/1.1 200 OK~%")
  (format client-stream "Content-Length: 0~%")
  (format client-stream "~%")
  (force-output client-stream))

(defun handle-update-post (client-stream &optional (app-name *default-application-name*))
  "Handle update request."
  (broadcast-update app-name)
  (format client-stream "HTTP/1.1 200 OK~%")
  (format client-stream "Content-Length: 0~%")
  (format client-stream "~%")
  (force-output client-stream))

(defun send-content-update (client-stream &optional (app-name *default-application-name*))
  "Send current application content to client via SSE."
  (let ((app (get-application app-name)))
    (when app
      (let* ((rendered-content (render-application app))
             (response (jsown:to-json (jsown:new-js ("type" "update") ("content" rendered-content)))))
        (ignore-errors
          (format client-stream "data: ~A~%~%" response)
          (force-output client-stream))))))

(defun broadcast-update (&optional (app-name *default-application-name*))
  "Send content update to all connected clients."
  (when *web-ui-instance*
    (dolist (client (client-connections *web-ui-instance*))
      (ignore-errors (send-content-update client app-name)))))

(defun handle-key-input (key-data &optional (app-name *default-application-name*))
  "Handle key input from POST request."
  (let ((key (jsown:val key-data "key"))
        (ctrl (jsown:val key-data "ctrl"))
        (alt (jsown:val key-data "alt")))
    (declare (ignore ctrl alt))
    (format t "Key event received for app '~A'. Key: '~A'. (Buffer modification is not implemented)~%" app-name key)
    (broadcast-update app-name)))

(defun handle-client (client-stream)
  "Handle a client connection."
  (let ((connection-status (handle-http-request client-stream)))
    (when (eq connection-status :close)
      ;; Small delay to ensure response is sent before closing
      (sleep 0.1))))

(defun start-server (port)
  "Start the web server."
  (let ((server-socket (usocket:socket-listen "0.0.0.0" port :reuse-address t)))
    (setf (server-socket *web-ui-instance*) server-socket)
    (setf *server-running* t)
    (bt:make-thread 
     (lambda ()
       (loop while *server-running* do
         (let ((client-socket (usocket:socket-accept server-socket)))
           (bt:make-thread 
            (lambda () 
              (unwind-protect
                  (handle-client (usocket:socket-stream client-socket))
                (usocket:socket-close client-socket)))))))
     :name "web-server")))

(defmethod run-ui ((ui web-ui) (editor editor))
  (setf *editor-instance* editor)
  (setf *web-ui-instance* ui)
  ;; Initialize default application
  (let ((default-app (make-instance 'application 
                                    :name *default-application-name*
                                    :editor editor)))
    (setf (gethash *default-application-name* *applications*) default-app))
  (start-server 8080)
  (format t "~%Your TLE application is now running.~%")
  (format t "Please open your web browser to http://localhost:8080~%")
  (format t "Use ?app=<name> parameter to access different applications~%")
  (loop (sleep 1)))

(defun make-web-ui ()
  (make-instance 'web-ui))

