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
        .menu-bar {
            background-color: #21252b;
            border-bottom: 1px solid #5c6370;
            height: 30px;
            display: flex;
            align-items: center;
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            z-index: 1000;
            user-select: none;
        }
        .menu-item {
            padding: 5px 12px;
            cursor: pointer;
            position: relative;
        }
        .menu-item:hover {
            background-color: #3e4451;
        }
        .menu-dropdown {
            position: absolute;
            top: 100%;
            left: 0;
            background-color: #3e4451;
            border: 1px solid #5c6370;
            border-radius: 3px;
            min-width: 150px;
            box-shadow: 0 4px 8px rgba(0,0,0,0.3);
            display: none;
            z-index: 1001;
        }
        .menu-dropdown.show {
            display: block;
        }
        .menu-option {
            padding: 8px 16px;
            cursor: pointer;
            border-bottom: 1px solid #5c6370;
        }
        .menu-option:last-child {
            border-bottom: none;
        }
        .menu-option:hover {
            background-color: #4b5263;
        }
        #app-container {
            margin-top: 30px;
            height: calc(100vh - 30px);
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
            display: flex;
            justify-content: space-between;
            align-items: center;
            transition: background-color 0.2s ease;
        }
        .window.focused .window-header {
            background-color: #5a647a;
        }
        .window.unfocused .window-header {
            background-color: #4b5263;
        }
        .window-close-btn {
            background: #e06c75;
            color: white;
            border: none;
            width: 20px;
            height: 20px;
            border-radius: 3px;
            cursor: pointer;
            font-size: 12px;
            font-weight: bold;
            display: flex;
            align-items: center;
            justify-content: center;
            margin-left: 10px;
            flex-shrink: 0;
        }
        .window-close-btn:hover {
            background: #c86975;
        }
        .window-content {
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
        .buffer-content {
            font-family: monospace;
            font-size: 14px;
            line-height: 1.4;
        }
        .line {
            display: flex;
            white-space: pre;
        }
        .line-number {
            color: #5c6370;
            background-color: #21252b;
            padding: 0 8px;
            margin-right: 10px;
            border-right: 1px solid #5c6370;
            min-width: 40px;
            text-align: right;
            user-select: none;
            flex-shrink: 0;
        }
        .line-content {
            flex: 1;
            padding-left: 5px;
        }
        .cursor {
            background-color: #56b6c2;
            color: #282c34;
        }
        .mark {
        }
        .selection {
            background-color: #3e6451;
            color: #abb2bf;
        }
        .selection.cursor {
            background-color: #56b6c2;
            color: #282c34;
        }
        .selection.mark {
        }
        .editor-pane {
            position: relative;
            height: 100%;
            display: flex;
            flex-direction: column;
        }
        .editor-content {
            flex: 1;
            overflow: auto;
            padding-bottom: 60px; /* Space for modeline and minibuffer */
        }
        .editor-bottom {
            position: absolute;
            bottom: 0;
            left: 0;
            right: 0;
            border-top: 1px solid #5c6370;
        }
        .modeline {
            color: #abb2bf;
            padding: 4px 10px;
            font-size: 12px;
            border-bottom: 1px solid #5c6370;
            user-select: none;
        }
        .minibuffer {
            color: #abb2bf;
            padding: 4px 10px;
            font-size: 14px;
            min-height: 20px;
            display: flex;
            align-items: center;
        }
        .minibuffer-prompt {
            color: #e06c75;
            margin-right: 5px;
        }
        .minibuffer-content {
            color: #abb2bf;
            flex: 1;
        }
    </style>
</head>
<body>
    <div class=\"menu-bar\">
        <div class=\"menu-item\" id=\"file-menu\">
            File
            <div class=\"menu-dropdown\" id=\"file-dropdown\">
                <div class=\"menu-option\" onclick=\"createNewWindow()\">New Window</div>
            </div>
        </div>
    </div>
    <div id=\"app-container\">~A</div>
    <script>
        const MENU_BAR_HEIGHT = 30;
        let currentZIndex = 1000;
        let focusedWindow = null;
        let eventSource = null;
        let draggedWindow = null;
        let dragOffset = {x: 0, y: 0};
        let resizedWindow = null;
        
        function makeWindowDraggable(windowElement) {
            const header = windowElement.querySelector('.window-header');
            const resizer = windowElement.querySelector('.window-resizer');
            const closeBtn = windowElement.querySelector('.window-close-btn');
            
            header.addEventListener('mousedown', startDrag);
            resizer.addEventListener('mousedown', startResize);
            closeBtn.addEventListener('click', closeFrame);
            
            // Bring window to front and focus when clicked anywhere
            windowElement.addEventListener('mousedown', (e) => {
                bringToFront(windowElement);
                setWindowFocus(windowElement);
            });
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
            const y = e.clientY - dragOffset.y - MENU_BAR_HEIGHT;
            
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
        
        function closeFrame(e) {
            e.preventDefault();
            e.stopPropagation();
            
            const windowElement = e.target.closest('.window');
            const frameId = windowElement.dataset.frameId;
            
            fetch('/frame-close', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({ frameId: frameId })
            });
        }
        
        function createNewWindow() {
            fetch('/frame-new', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({})
            });
            hideAllMenus();
        }
        
        function showMenu(menuId) {
            hideAllMenus();
            document.getElementById(menuId).classList.add('show');
        }
        
        function hideAllMenus() {
            document.querySelectorAll('.menu-dropdown').forEach(menu => {
                menu.classList.remove('show');
            });
        }
        
        function bringToFront(windowElement) {
            currentZIndex++;
            windowElement.style.zIndex = currentZIndex;
            
            // Send z-index update to server
            const frameId = windowElement.dataset.frameId;
            fetch('/frame-zindex', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({ frameId: frameId, zIndex: currentZIndex })
            });
        }
        
        function setWindowFocus(windowElement) {
            // Remove focus from previously focused window
            if (focusedWindow) {
                focusedWindow.classList.remove('focused');
                focusedWindow.classList.add('unfocused');
            }
            
            // Set focus on new window
            focusedWindow = windowElement;
            windowElement.classList.remove('unfocused');
            windowElement.classList.add('focused');
            
            // Send focus update to server
            const frameId = windowElement.dataset.frameId;
            fetch('/frame-focus', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({ frameId: frameId })
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
        
        // Menu event handlers
        document.addEventListener('click', (e) => {
            if (!e.target.closest('.menu-bar')) {
                hideAllMenus();
            }
        });
        
        document.getElementById('file-menu').addEventListener('click', (e) => {
            e.stopPropagation();
            const dropdown = document.getElementById('file-dropdown');
            if (dropdown.classList.contains('show')) {
                hideAllMenus();
            } else {
                showMenu('file-dropdown');
            }
        });
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

(defmethod render ((frame frame) (ui web-ui))
  "Render a frame as HTML for web UI."
  (let* ((frame-content (if (typep frame 'standard-frame)
                           (render-components frame ui)
                           (escape-html (format nil "Frame: ~A" frame))))
         (frame-id (symbol-name (frame-id frame)))
         (title (if (typep frame 'standard-frame) (frame-title frame) "Frame"))
         (x (if (typep frame 'standard-frame) (frame-x frame) 50))
         (y (if (typep frame 'standard-frame) (frame-y frame) 50))
         (width (if (typep frame 'standard-frame) (frame-width frame) 400))
         (height (if (typep frame 'standard-frame) (frame-height frame) 300))
         (z-index (if (typep frame 'standard-frame) (frame-z-index frame) 1000))
         (focused (if (typep frame 'standard-frame) (frame-focused frame) nil))
         (focus-class (if focused "focused" "unfocused")))
    (format nil "<div class=\"window ~A\" data-frame-id=\"~A\" style=\"left: ~Apx; top: ~Apx; width: ~Apx; height: ~Apx; z-index: ~A;\">
  <div class=\"window-header\">
    <span>~A</span>
    <button class=\"window-close-btn\" title=\"Close\">&#215;</button>
  </div>
  <div class=\"window-content\">~A</div>
  <div class=\"window-resizer\"></div>
</div>" focus-class frame-id x y width height z-index title frame-content)))

(defmethod render ((app application) (ui web-ui))
  "Render an application as HTML with all its frames as draggable windows."
  (let ((frames (application-frames app))
        (editor (application-editor app)))
    (if (null frames)
        ;; If no frames, create a default window with the current editor
        (let* ((content (if editor (render editor ui) "No content"))
               (frame-id "default-frame")
               (title (format nil "~A - Buffer" (application-name app)))
               (focus-class "focused"))
          (format nil "<div class=\"window ~A\" data-frame-id=\"~A\" style=\"left: ~Apx; top: ~Apx; width: ~Apx; height: ~Apx; z-index: ~A;\">
  <div class=\"window-header\">
    <span>~A</span>
    <button class=\"window-close-btn\" title=\"Close\">&#215;</button>
  </div>
  <div class=\"window-content\">~A</div>
  <div class=\"window-resizer\"></div>
</div>" focus-class frame-id 50 50 600 400 1000 title content))
        ;; Render all frames as windows using their stored coordinates
        (let ((window-html ""))
          (dolist (frame frames)
            (setf window-html
                  (concatenate 'string window-html (render frame ui))))
          window-html))))

(defun write-line-crlf (stream &optional (line ""))
  "Write a line to stream with proper CRLF termination."
  (write-string line stream)
  (write-char #\Return stream)
  (write-char #\Linefeed stream))

(defun send-http-response (client-stream status-line &optional headers body)
  "Send a complete HTTP response with status, headers, and optional body."
  (write-line-crlf client-stream status-line)
  (when headers
    (dolist (header headers)
      (write-line-crlf client-stream header)))
  ;; Add Content-Length header if not already present and we have a body or need explicit 0
  (when (not (member "Content-Length" headers :test (lambda (search header) (search search header))))
    (write-line-crlf client-stream (format nil "Content-Length: ~A" (if body (length body) 0))))
  (write-line-crlf client-stream)
  (when body
    (write-string body client-stream))
  (force-output client-stream))

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
                    ((and (string= method "POST") (string= path "/frame-close"))
                     (handle-frame-close-post client-stream body app-name)
                     :close)
                    ((and (string= method "POST") (string= path "/frame-new"))
                     (handle-frame-new-post client-stream body app-name)
                     :close)
                    ((and (string= method "POST") (string= path "/frame-zindex"))
                     (handle-frame-zindex-post client-stream body app-name)
                     :close)
                    ((and (string= method "POST") (string= path "/frame-focus"))
                     (handle-frame-focus-post client-stream body app-name)
                     :close)
                    (t
                     (send-404-response client-stream)
                     :close)))))
            :close))
    (error (e)
      (format t "Error handling request:~%")
      (describe e t)  ; This often includes stack information
      (ignore-errors (send-404-response client-stream))
      :close)))

(defun send-html-response (client-stream &optional (app-name *default-application-name*))
  "Send HTML page response."
  (format t "Sending HTML response for app: ~A~%" app-name)
  (let* ((app (get-application app-name))
         (rendered-content (if app (render app *web-ui-instance*) "<div id=\"editor\">No application found</div>"))
         (html-content (format nil *html-template* rendered-content)))
    (send-http-response client-stream "HTTP/1.1 200 OK" 
                       '("Content-Type: text/html" "Connection: close")
                       html-content)
    (finish-output client-stream)
    (format t "HTML response sent~%")))

(defun send-sse-response (client-stream &optional (app-name *default-application-name*))
  "Send Server-Sent Events response."
  (push client-stream (client-connections *web-ui-instance*))
  ;; SSE responses should not have Content-Length header
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
  (send-http-response client-stream "HTTP/1.1 404 Not Found"))

(defun handle-key-post (client-stream body &optional (app-name *default-application-name*))
  "Handle key input POST request."
  (when body
    (let* ((data (jsown:parse body)))
      (handle-key-input data app-name)))
  (send-http-response client-stream "HTTP/1.1 200 OK"))

(defun handle-update-post (client-stream &optional (app-name *default-application-name*))
  "Handle update request."
  (broadcast-update app-name)
  (send-http-response client-stream "HTTP/1.1 200 OK"))

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
  
  (send-http-response client-stream "HTTP/1.1 200 OK"))

(defun handle-frame-close-post (client-stream body &optional (app-name *default-application-name*))
  "Handle frame close request."
  (when body
    (let* ((data (jsown:parse body))
           (frame-id (jsown:val data "frameId")))
      (remove-frame-from-application app-name frame-id)
      (broadcast-update app-name)))
  (send-http-response client-stream "HTTP/1.1 200 OK"))

(defun handle-frame-new-post (client-stream body &optional (app-name *default-application-name*))
  "Handle new frame creation request."
  (declare (ignore body))
  (create-new-frame-in-application app-name)
  (broadcast-update app-name)
  (send-http-response client-stream "HTTP/1.1 200 OK"))

(defun handle-frame-zindex-post (client-stream body &optional (app-name *default-application-name*))
  "Handle frame z-index update request."
  (when body
    (let* ((data (jsown:parse body))
           (frame-id (jsown:val data "frameId"))
           (z-index (jsown:val data "zIndex")))
      (update-frame-z-index app-name frame-id z-index)))
  (send-http-response client-stream "HTTP/1.1 200 OK"))

(defun handle-frame-focus-post (client-stream body &optional (app-name *default-application-name*))
  "Handle frame focus update request."
  (when body
    (let* ((data (jsown:parse body))
           (frame-id (jsown:val data "frameId")))
      (update-application-frame-focus app-name frame-id)))
  (send-http-response client-stream "HTTP/1.1 200 OK"))

(defun send-content-update (client-stream &optional (app-name *default-application-name*))
  "Send current application content to client via SSE."
  (let ((app (get-application app-name)))
    (when app
      (let* ((rendered-content (render app *web-ui-instance*))
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

(defun remove-frame-from-application (app-name frame-id)
  "Remove a frame from the application."
  (let ((app (get-application app-name)))
    (when app
      (let ((frames-before (length (application-frames app))))
        (setf (application-frames app)
              (remove-if (lambda (frame)
                          (string= (symbol-name (frame-id frame)) frame-id))
                        (application-frames app)))
        (let ((frames-after (length (application-frames app))))
          (format t "Removed frame ~A from app ~A. Frames: ~A -> ~A~%" 
                  frame-id app-name frames-before frames-after))))))

(defun create-new-frame-in-application (app-name)
  "Create a new frame in the application."
  (let ((app (get-application app-name)))
    (when app
      (let* ((editor (application-editor app))
             (buffer (when editor (current-buffer editor)))
             (frame-id (gensym "FRAME"))
             (title (format nil "Buffer ~A" (length (application-frames app))))
             ;; Position new frames with slight offset
             (x (+ 100 (* 20 (length (application-frames app)))))
             (y (+ 100 (* 20 (length (application-frames app)))))
             ;; Find highest z-index among existing frames and add 1
             (highest-z-index (if (application-frames app)
                                 (reduce #'max (application-frames app) 
                                        :key #'frame-z-index)
                                 1000))
             (new-z-index (1+ highest-z-index))
             (new-frame (make-instance 'standard-frame
                                       :id frame-id
                                       :title title
                                       :buffer (or buffer (make-instance 'buffer))
                                       :x x
                                       :y y
                                       :width 400
                                       :height 300
                                       :z-index new-z-index
                                       :focused t)))
        ;; Unfocus all existing frames before adding the new focused frame
        (dolist (frame (application-frames app))
          (update-frame-focus frame nil))
        (push new-frame (application-frames app))
        (format t "Created new frame ~A in app ~A with z-index ~A. Total frames: ~A~%" 
                frame-id app-name new-z-index (length (application-frames app)))))))

(defun update-frame-z-index (app-name frame-id z-index)
  "Update frame z-index in the application."
  (let ((app (get-application app-name)))
    (when app
      (dolist (frame (application-frames app))
        (when (string= (symbol-name (frame-id frame)) frame-id)
          (update-frame-z-index-value frame z-index)
          (format t "Updated frame ~A z-index to ~A~%" frame-id z-index)
          (return))))))

(defun update-application-frame-focus (app-name focused-frame-id)
  "Update frame focus in the application - only one frame can be focused at a time."
  (let ((app (get-application app-name)))
    (when app
      (dolist (frame (application-frames app))
        (let ((frame-id (symbol-name (frame-id frame)))
              (should-be-focused (string= (symbol-name (frame-id frame)) focused-frame-id)))
          (update-frame-focus frame should-be-focused)
          (when should-be-focused
            (format t "Focused frame ~A~%" frame-id)))))))

(defun handle-key-input (key-data &optional (app-name *default-application-name*))
  "Handle key input from POST request."
  (let ((key (jsown:val key-data "key"))
        (ctrl (jsown:val key-data "ctrl"))
        (alt (jsown:val key-data "alt"))
        (shift (jsown:val key-data "shift")))
    (format t "Key event received for app '~A'. Key: '~A', Ctrl: ~A, Alt: ~A~%" app-name key ctrl alt)
    
    ;; Get the current buffer for the application
    (let* ((app (get-application app-name))
           (editor (when app (application-editor app)))
           (buffer (when editor (current-buffer editor))))
      
      (when editor
        ;; First check if minibuffer is active and should handle the input
        (if (and (minibuffer-active-p editor) 
                 (handle-minibuffer-input editor key ctrl alt shift))
            ;; Minibuffer handled the input, do nothing more
            (format t "Minibuffer handled key: ~A~%" key)
            ;; Normal key handling when minibuffer is not active or didn't handle the key
            (when buffer
              ;; Handle cursor movement keys
              (cond
          ;; Arrow keys
          ((string= key "ArrowUp")
           (previous-line buffer)
           (format t "Moved cursor up~%"))
          ((string= key "ArrowDown") 
           (next-line buffer)
           (format t "Moved cursor down~%"))
          ((string= key "ArrowLeft")
           (backward-char buffer)
           (format t "Moved cursor left~%"))
          ((string= key "ArrowRight")
           (forward-char buffer)
           (format t "Moved cursor right~%"))
          
          ;; Ctrl key combinations
          ((and ctrl (string= key "p"))
           (previous-line buffer)
           (format t "Ctrl-P: Moved cursor up~%"))
          ((and ctrl (string= key "n"))
           (next-line buffer)
           (format t "Ctrl-N: Moved cursor down~%"))
          ((and ctrl (string= key "b"))
           (backward-char buffer)
           (format t "Ctrl-B: Moved cursor left~%"))
          ((and ctrl (string= key "f"))
           (forward-char buffer)
           (format t "Ctrl-F: Moved cursor right~%"))
          ((and ctrl (string= key "a"))
           (beginning-of-line buffer)
           (format t "Ctrl-A: Moved cursor to beginning of line~%"))
          ((and ctrl (string= key "e"))
           (end-of-line buffer)
           (format t "Ctrl-E: Moved cursor to end of line~%"))
          ((and ctrl (string= key " "))
           (let ((point (buffer-get-point buffer)))
             (buffer-set-mark buffer (first point) (second point)))
           (format t "Ctrl-Space: Set mark at current position~%"))
          ((and ctrl (string= key "_"))
           (if (buffer-undo buffer)
               (format t "Ctrl-_: Undo operation performed~%")
               (format t "Ctrl-_: Nothing to undo~%")))
          ((and ctrl (string= key "?"))
           (if (buffer-redo buffer)
               (format t "Ctrl-?: Redo operation performed~%")
               (format t "Ctrl-?: Nothing to redo~%")))
          ((and ctrl (string= key "k"))
           (kill-line buffer)
           (format t "Ctrl-K: Kill line~%"))
          ((and ctrl shift (string= key "K"))
           (kill-whole-line buffer)
           (format t "Ctrl-Shift-K: Kill whole line~%"))
          ((and ctrl (string= key "d"))
           (kill-word buffer)
           (format t "Ctrl-D: Kill word~%"))
          ((and ctrl shift (string= key "D"))
           (delete-region buffer)
           (format t "Ctrl-Shift-D: Delete region~%"))
          ((and alt (string= key "Backspace"))
           (backward-kill-word buffer)
           (format t "Alt-Backspace: Backward kill word~%"))
          ((and ctrl (string= key "x"))
           (kill-region buffer)
           (format t "Ctrl-X: Kill region~%"))
          ((and ctrl (string= key "c"))
           (copy-region-as-kill buffer)
           (format t "Ctrl-C: Copy region as kill~%"))
          ((and ctrl (string= key "v"))
           (yank buffer)
           (format t "Ctrl-V: Yank from kill ring~%"))
          ((and alt (string= key "v"))
           (yank-pop buffer)
           (format t "Alt-V: Yank pop (cycle kill ring)~%"))
          ((and alt (string= key "f"))
           (forward-word buffer)
           (format t "Alt-F: Forward word~%"))
          ((and alt (string= key "b"))
           (backward-word buffer)
           (format t "Alt-B: Backward word~%"))
          ((and alt (string= key "x"))
           (execute-command editor)
           (format t "Alt-X: Execute command~%"))
          ((and alt (string= key "a"))
           (move-beginning-of-line buffer)
           (format t "Alt-A: Smart move to beginning of line~%"))
          ((and alt shift (string= key "B"))
           (beginning-of-word buffer)
           (format t "Alt-Shift-B: Beginning of word~%"))
          ((and alt shift (string= key "E"))
           (end-of-word buffer)
           (format t "Alt-Shift-E: End of word~%"))
          ((and alt (string= key "<"))
           (beginning-of-buffer buffer)
           (format t "Alt-<: Move to beginning of buffer~%"))
          ((and alt (string= key ">"))
           (end-of-buffer buffer)
           (format t "Alt->: Move to end of buffer~%"))
          ((and alt (string= key "g"))
           ;; For now, hardcoded to line 5 for testing - in a real implementation,
           ;; this would prompt the user for a line number
           (goto-line buffer 5)
           (format t "Alt-g: Go to line 5 (hardcoded for demo)~%"))
          
          ;; Enter key for newline insertion
          ((string= key "Enter")
           (insert-newline buffer)
           (format t "Inserted newline~%"))
          
          ;; Delete key for character deletion
          ((string= key "Delete")
           (delete-char buffer)
           (format t "Deleted character at point~%"))
          
          ;; Backspace key for backward character deletion
          ((string= key "Backspace")
           (delete-backward-char buffer)
           (format t "Deleted character before point~%"))
          
          ;; Printable characters (single character keys that are not special)
          ((and (= (length key) 1)
                (not ctrl)
                (not alt)
                (graphic-char-p (char key 0)))
           (insert-char buffer (char key 0))
           (format t "Inserted character: ~A~%" key))
          
          ;; Default case for unhandled keys
          (t
           (format t "Unhandled key: ~A~%" key))))))
      
      ;; Always broadcast update to refresh the display
      (broadcast-update app-name))))

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

