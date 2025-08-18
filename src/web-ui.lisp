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
            if (draggedWindow) {
                const frameId = draggedWindow.dataset.frameId;
                const rect = draggedWindow.getBoundingClientRect();
                sendFrameUpdate(frameId, {
                    x: parseInt(draggedWindow.style.left),
                    y: parseInt(draggedWindow.style.top),
                    width: parseInt(draggedWindow.style.width) || rect.width,
                    height: parseInt(draggedWindow.style.height) || rect.height
                });
            }
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
            if (resizedWindow) {
                const frameId = resizedWindow.dataset.frameId;
                const rect = resizedWindow.getBoundingClientRect();
                sendFrameUpdate(frameId, {
                    x: parseInt(resizedWindow.style.left) || rect.left,
                    y: parseInt(resizedWindow.style.top) || rect.top,
                    width: parseInt(resizedWindow.style.width),
                    height: parseInt(resizedWindow.style.height)
                });
            }
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
        
        function sendFrameUpdate(frameId, position) {
            fetch('/frame-update', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({
                    frameId: frameId,
                    x: position.x,
                    y: position.y,
                    width: position.width,
                    height: position.height
                })
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
               (content (if buffer (escape-html (get-buffer-text buffer)) "No content"))
               (frame-id "default-frame"))
          (format nil "<div class=\"window\" data-frame-id=\"~A\" style=\"left: 50px; top: 50px; width: 600px; height: 400px;\">
  <div class=\"window-header\">~A - Buffer</div>
  <div class=\"window-content\">~A</div>
  <div class=\"window-resizer\"></div>
</div>" frame-id (application-name app) content))
        ;; Render all frames as windows using their stored coordinates
        (let ((window-html ""))
          (dolist (frame frames)
            (let* ((frame-content (if (typep frame 'standard-frame)
                                     (escape-html (get-buffer-text (frame-buffer frame)))
                                     (escape-html (format nil "Frame: ~A" frame))))
                   (frame-id (symbol-name (frame-id frame)))
                   (title (if (typep frame 'standard-frame) (frame-title frame) "Frame"))
                   (x (if (typep frame 'standard-frame) (frame-x frame) 50))
                   (y (if (typep frame 'standard-frame) (frame-y frame) 50))
                   (width (if (typep frame 'standard-frame) (frame-width frame) 400))
                   (height (if (typep frame 'standard-frame) (frame-height frame) 300)))
              (setf window-html
                    (concatenate 'string window-html
                                 (format nil "<div class=\"window\" data-frame-id=\"~A\" style=\"left: ~Apx; top: ~Apx; width: ~Apx; height: ~Apx;\">
  <div class=\"window-header\">~A</div>
  <div class=\"window-content\">~A</div>
  <div class=\"window-resizer\"></div>
</div>" frame-id x y width height title frame-content)))))
          window-html))))

(defun write-line-crlf (stream &optional (line ""))
  "Write a line to stream with proper CRLF termination."
  (write-string line stream)
  (write-char #\Return stream)
  (write-char #\Linefeed stream))

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
                   ;;(format t "Read header line: '~A'~%" line)
                   (when (or (string= line "") (string= line (string #\Return)))
                     ;;(format t "Found empty line, ending header parsing~%")
                     (return))
                   (let ((colon-pos (position #\: line)))
                     (when colon-pos
                       (push (cons (string-trim " " (subseq line 0 colon-pos))
                                   (string-trim '(#\Space #\Tab #\Return #\Linefeed) (subseq line (1+ colon-pos))))
                             headers)))))
        (setf headers (nreverse headers))
        (format t "Headers read: ~S~%" headers)
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
                    ((and (string= method "POST") (string= path "/frame-update"))
                     (handle-frame-update-post client-stream body app-name)
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
         (content-length (length html-content)))
    (write-line-crlf client-stream "HTTP/1.1 200 OK")
    (write-line-crlf client-stream "Content-Type: text/html")
    (write-line-crlf client-stream (format nil "Content-Length: ~A" content-length))
    (write-line-crlf client-stream "Connection: close")
    (write-line-crlf client-stream)
    (write-string html-content client-stream)
    (force-output client-stream)
    (finish-output client-stream)
    (format t "HTML response sent~%")))

(defun send-sse-response (client-stream &optional (app-name *default-application-name*))
  "Send Server-Sent Events response."
  (push client-stream (client-connections *web-ui-instance*))
  (write-line-crlf client-stream "HTTP/1.1 200 OK")
  (write-line-crlf client-stream "Content-Type: text/event-stream")
  (write-line-crlf client-stream "Cache-Control: no-cache")
  (write-line-crlf client-stream "Connection: keep-alive")
  (write-line-crlf client-stream "Access-Control-Allow-Origin: *")
  (write-line-crlf client-stream)
  (force-output client-stream)
  
  ;; Send initial content
  (send-content-update client-stream app-name)
  
  ;; Keep connection alive in background
  (bt:make-thread
   (lambda ()
     (handler-case
         (loop
           (sleep 30)
           (handler-case
               (progn
                 (write-line-crlf client-stream ": heartbeat")
                 (write-line-crlf client-stream)
                 (force-output client-stream))
             (error (e)
               (format t "SSE connection error, removing client: ~A~%" e)
               (setf (client-connections *web-ui-instance*)
                     (remove client-stream (client-connections *web-ui-instance*)))
               (return))))
       (error (e)
         (format t "SSE thread error: ~A~%" e)
         (setf (client-connections *web-ui-instance*)
               (remove client-stream (client-connections *web-ui-instance*))))))
   :name "sse-keepalive"))

(defun send-404-response (client-stream)
  "Send 404 Not Found response."
  (write-line-crlf client-stream "HTTP/1.1 404 Not Found")
  (write-line-crlf client-stream "Content-Length: 0")
  (write-line-crlf client-stream)
  (force-output client-stream))

(defun handle-key-post (client-stream body &optional (app-name *default-application-name*))
  "Handle key input POST request."
  (when body
    (let* ((data (jsown:parse body)))
      (handle-key-input data app-name)))
  
  (write-line-crlf client-stream "HTTP/1.1 200 OK")
  (write-line-crlf client-stream "Content-Length: 0")
  (write-line-crlf client-stream)
  (force-output client-stream))

(defun handle-update-post (client-stream &optional (app-name *default-application-name*))
  "Handle update request."
  (broadcast-update app-name)
  (write-line-crlf client-stream "HTTP/1.1 200 OK")
  (write-line-crlf client-stream "Content-Length: 0")
  (write-line-crlf client-stream)
  (force-output client-stream))

(defun handle-frame-update-post (client-stream body &optional (app-name *default-application-name*))
  "Handle frame position/size update request."
  (when body
    (let* ((data (jsown:parse body))
           (frame-id (jsown:val data "frameId"))
           (x (jsown:val data "x"))
           (y (jsown:val data "y"))
           (width (jsown:val data "width"))
           (height (jsown:val data "height")))
      (update-frame-position-and-size app-name frame-id x y width height)))
  
  (write-line-crlf client-stream "HTTP/1.1 200 OK")
  (write-line-crlf client-stream "Content-Length: 0")
  (write-line-crlf client-stream)
  (force-output client-stream))

(defun send-content-update (client-stream &optional (app-name *default-application-name*))
  "Send current application content to client via SSE."
  (let ((app (get-application app-name)))
    (when app
      (let* ((rendered-content (render-application app))
             (response (jsown:to-json (jsown:new-js ("type" "update") ("content" rendered-content)))))
        (handler-case
            (progn
              (write-line-crlf client-stream (format nil "data: ~A" response))
              (write-line-crlf client-stream)
              (force-output client-stream))
          (error (e)
            (format t "Error sending content update: ~A~%" e)
            (setf (client-connections *web-ui-instance*)
                  (remove client-stream (client-connections *web-ui-instance*)))))))))

(defun broadcast-update (&optional (app-name *default-application-name*))
  "Send content update to all connected clients."
  (when *web-ui-instance*
    (let ((clients-to-remove '()))
      (dolist (client (client-connections *web-ui-instance*))
        (handler-case
            (send-content-update client app-name)
          (error (e)
            (format t "Error broadcasting to client, will remove: ~A~%" e)
            (push client clients-to-remove))))
      ;; Remove failed clients
      (dolist (client clients-to-remove)
        (setf (client-connections *web-ui-instance*)
              (remove client (client-connections *web-ui-instance*)))))))

(defun update-frame-position-and-size (app-name frame-id x y width height)
  "Update frame position and size in the application."
  (let ((app (get-application app-name)))
    (when app
      (dolist (frame (application-frames app))
        (when (string= (symbol-name (frame-id frame)) frame-id)
          (update-frame-position frame x y)
          (update-frame-size frame width height)
          (format t "Updated frame ~A: position (~A,~A) size (~A,~A)~%" 
                  frame-id x y width height)
          (return))))))

(defun handle-key-input (key-data &optional (app-name *default-application-name*))
  "Handle key input from POST request."
  (let ((key (jsown:val key-data "key"))
        (ctrl (jsown:val key-data "ctrl"))
        (alt (jsown:val key-data "alt")))
    (declare (ignore ctrl alt))
    (format t "Key event received for app '~A'. Key: '~A'. (Buffer modification is not implemented)~%" app-name key)
    (broadcast-update app-name)))

(defun handle-client (client-socket)
  "Handle a client connection."
  (let* ((client-stream (usocket:socket-stream client-socket))
         (connection-status (handle-http-request client-stream)))
    (case connection-status
      (:close 
       ;; Small delay to ensure response is sent before closing
       (sleep 0.1)
       (usocket:socket-close client-socket))
      (:keep-alive
       ;; Don't close the socket for SSE connections
       nil))))

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
              (handle-client client-socket))))))
     :name "web-server")))

(defmethod run-ui ((ui web-ui) (editor editor))
  (setf *editor-instance* editor)
  (setf *web-ui-instance* ui)
  ;; Initialize default application
  (let ((default-app (make-instance 'application 
                                    :name *default-application-name*
                                    :editor editor)))
    (setf (gethash *default-application-name* *applications*) default-app)
    ;; Create sample frames for testing
    (create-sample-frames *default-application-name*))
  (start-server 8080)
  (format t "~%Your TLE application is now running.~%")
  (format t "Please open your web browser to http://localhost:8080~%")
  (format t "Use ?app=<name> parameter to access different applications~%")
  (loop (sleep 1)))

(defun make-web-ui ()
  (make-instance 'web-ui))

