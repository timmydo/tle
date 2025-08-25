(in-package :tle)

(defclass editor ()
  ()
  (:documentation "An editor implementation"))

(defgeneric editor-buffers (editor)
  (:documentation "Get an editors buffers"))

(defclass standard-editor (editor)
  ((%buffers :accessor buffers)
   (%minibuffer :accessor minibuffer :initform nil)
   (%minibuffer-active :accessor minibuffer-active-p :initform nil)
   (%minibuffer-prompt :accessor minibuffer-prompt :initform "")
   (%minibuffer-completion-function :accessor minibuffer-completion-function :initform nil)
   (%minibuffer-callback :accessor minibuffer-callback :initform nil)
   (%minibuffer-live-callback :accessor minibuffer-live-callback :initform nil)
   (%isearch-original-position :accessor isearch-original-position :initform nil)))

(defun make-standard-editor ()
  (let ((e (make-instance 'standard-editor)))
    (setf (buffers e) (list (make-standard-buffer "*scratch*")))
    e))

(defmethod editor-buffers ((editor standard-editor))
  (buffers editor))

(defun current-buffer (editor)
  "Get the current buffer from the editor."
  (first (buffers editor)))

(defun get-modeline-info (editor)
  "Get modeline information (filename, line, column) for the current buffer."
  (let ((buffer (current-buffer editor)))
    (if buffer
        (let ((point (buffer-get-point buffer)))
          (list :filename (buffer-name buffer)
                :line (1+ (first point))
                :column (1+ (second point))))
        (list :filename "No buffer" :line 0 :column 0))))

(defun activate-minibuffer (editor prompt &optional completion-function callback-function live-callback-function)
  "Activate the minibuffer with the given prompt, optional completion function, callback function, and live callback function."
  (setf (minibuffer editor) (make-empty-buffer "*minibuffer*"))
  (setf (minibuffer-active-p editor) t)
  (setf (minibuffer-prompt editor) prompt)
  (setf (minibuffer-completion-function editor) completion-function)
  (setf (minibuffer-callback editor) callback-function)
  (setf (minibuffer-live-callback editor) live-callback-function))

(defun deactivate-minibuffer (editor)
  "Deactivate the minibuffer and return its contents."
  (let ((contents (if (minibuffer editor)
                      (buffer-line (minibuffer editor) 0)
                      "")))
    (setf (minibuffer editor) nil)
    (setf (minibuffer-active-p editor) nil)
    (setf (minibuffer-prompt editor) "")
    (setf (minibuffer-completion-function editor) nil)
    (setf (minibuffer-callback editor) nil)
    (setf (minibuffer-live-callback editor) nil)
    contents))

(defun get-minibuffer-contents (editor)
  "Get the current contents of the minibuffer."
  (if (and (minibuffer editor) (minibuffer-active-p editor))
      (buffer-line (minibuffer editor) 0)
      ""))

(defun trigger-minibuffer-live-callback (editor)
  "Trigger the live callback if one is set."
  (let ((live-callback (minibuffer-live-callback editor)))
    (when live-callback
      (let ((contents (get-minibuffer-contents editor)))
        (funcall live-callback contents editor)))))

(defmethod render ((editor standard-editor) (ui ui-implementation))
  "Render a standard editor component with modeline and minibuffer."
  (let ((current-buf (current-buffer editor))
        (modeline-info (get-modeline-info editor)))
    (format nil "<div class=\"editor-pane\">~A<div class=\"editor-bottom\">~A~A</div></div>"
            (if current-buf
                (format nil "<div class=\"editor-content\">~A</div>"
                        (render current-buf ui))
                "<div class=\"editor-content\">No buffer available</div>")
            (render-modeline modeline-info)
            (render-minibuffer editor ui))))

(defun render-modeline (modeline-info)
  "Render the modeline showing filename, line, and column."
  (format nil "<div class=\"modeline\">~A Line:~D Col:~D</div>"
          (getf modeline-info :filename)
          (getf modeline-info :line)
          (getf modeline-info :column)))

(defun render-minibuffer (editor &optional ui)
  "Render the minibuffer - always visible, no line numbers."
  (let ((prompt (if (minibuffer-active-p editor) 
                    (minibuffer-prompt editor) 
                    ""))
        (content (if (and (minibuffer editor) (minibuffer-active-p editor))
                     (buffer-line (minibuffer editor) 0)
                     "")))
    (format nil "<div class=\"minibuffer\"><span class=\"minibuffer-prompt\">~A</span><span class=\"minibuffer-content\">~A</span></div>"
            prompt content)))

(defvar *command-table* (make-hash-table :test 'equal)
  "Hash table mapping command names to functions.")

(defun register-command (name function)
  "Register a command in the command table."
  (setf (gethash name *command-table*) function))

(defun execute-command (editor)
  "Start M-x command execution by activating the minibuffer."
  (activate-minibuffer editor "M-x " #'complete-command-name 
                       (lambda (command-name editor)
                         (execute-named-command command-name editor))))

(defun complete-command-name (partial-name)
  "Return a list of command names that match the partial name."
  (let ((matches '()))
    (maphash (lambda (name function)
               (declare (ignore function))
               (when (and (>= (length name) (length partial-name))
                          (string= partial-name name :end2 (length partial-name)))
                 (push name matches)))
             *command-table*)
    (sort matches #'string<)))

(defun execute-named-command (command-name editor)
  "Execute a named command."
  (let ((command-function (gethash command-name *command-table*)))
    (if command-function
        (funcall command-function editor)
        (format t "Unknown command: ~A~%" command-name))))

(defun handle-minibuffer-input (editor key ctrl alt shift)
  "Handle input when minibuffer is active."
  (let ((minibuf (minibuffer editor)))
    (cond
      ;; Enter key - execute callback or command
      ((string= key "Enter")
       (let ((contents (get-minibuffer-contents editor))
             (callback (minibuffer-callback editor)))
         (deactivate-minibuffer editor)
         (if callback
             (funcall callback contents editor)
             (execute-named-command contents editor)))
       t)
      
      ;; Escape key - cancel minibuffer
      ((string= key "Escape")
       ;; If this is an isearch, restore original position
       (let ((original-pos (isearch-original-position editor))
             (buffer (current-buffer editor)))
         (when (and original-pos buffer)
           (clear-isearch-state buffer)
           (buffer-set-point buffer (first original-pos) (second original-pos))
           (setf (isearch-original-position editor) nil)
           (format t "Isearch cancelled, position restored~%")))
       (deactivate-minibuffer editor)
       (format t "Minibuffer cancelled~%")
       t)
      
      ;; Backspace - delete character in minibuffer
      ((string= key "Backspace")
       (delete-backward-char minibuf)
       (trigger-minibuffer-live-callback editor)
       t)
      
      ;; Delete - delete character at point in minibuffer
      ((string= key "Delete")
       (delete-char minibuf)
       (trigger-minibuffer-live-callback editor)
       t)
      
      ;; Regular characters - insert into minibuffer
      ((and (= (length key) 1)
            (not ctrl)
            (not alt)
            (graphic-char-p (char key 0)))
       (insert-char minibuf (char key 0))
       (trigger-minibuffer-live-callback editor)
       t)
      
      ;; Other keys are not handled by minibuffer
      (t nil))))

(defun prompt-goto-line (editor)
  "Prompt user for line number and execute goto-line."
  (activate-minibuffer editor "Goto line: " nil 
                       (lambda (line-string editor)
                         (let ((line-number (ignore-errors (parse-integer (string-trim " " line-string)))))
                           (if line-number
                               (let ((buffer (current-buffer editor)))
                                 (when buffer
                                   (goto-line buffer line-number)
                                   (format t "Goto line ~A~%" line-number)))
                               (format t "Invalid line number: ~A~%" line-string))))))

(defun search-forward-command (search-string editor)
  "Execute search-forward with the given search string."
  (let ((buffer (current-buffer editor)))
    (when buffer
      (if (search-forward buffer (string-trim " " search-string))
          (format t "Found: ~A~%" search-string)
          (format t "Not found: ~A~%" search-string)))))

(defun search-backward-command (search-string editor)
  "Execute search-backward with the given search string."
  (let ((buffer (current-buffer editor)))
    (when buffer
      (if (search-backward buffer (string-trim " " search-string))
          (format t "Found: ~A~%" search-string)
          (format t "Not found: ~A~%" search-string)))))

(defun isearch-forward-command (search-string editor)
  "Execute isearch-forward with the given search string."
  (let ((buffer (current-buffer editor)))
    (when buffer
      (if (isearch-forward buffer (string-trim " " search-string))
          (format t "Found: ~A~%" search-string)
          (format t "Not found: ~A~%" search-string)))))

(defun live-isearch-forward (search-string editor)
  "Live incremental search forward - called as user types in minibuffer."
  (let ((buffer (current-buffer editor)))
    (when buffer
      (let ((trimmed-string (string-trim " " search-string)))
        (if (and trimmed-string (> (length trimmed-string) 0))
            ;; Search for the string
            (let ((original-pos (isearch-original-position editor)))
              ;; Set isearch state for highlighting
              (set-isearch-state buffer trimmed-string t)
              ;; Restore to original position first
              (when original-pos
                (buffer-set-point buffer (first original-pos) (second original-pos)))
              ;; Perform search from original position
              (if (isearch-forward buffer trimmed-string)
                  (format t "Isearch: Found '~A'~%" trimmed-string)
                  (format t "Isearch: '~A' not found~%" trimmed-string)))
            ;; Empty search string - restore original position and clear isearch
            (progn
              (clear-isearch-state buffer)
              (let ((original-pos (isearch-original-position editor)))
                (when original-pos
                  (buffer-set-point buffer (first original-pos) (second original-pos))
                  (format t "Isearch: Restored to original position~%")))))))))

(defun start-isearch-forward (editor)
  "Start incremental search forward with live updates."
  (let ((buffer (current-buffer editor)))
    (when buffer
      ;; Store original cursor position
      (setf (isearch-original-position editor) (copy-list (buffer-get-point buffer)))
      ;; Activate minibuffer with live callback
      (activate-minibuffer editor "I-search: " nil 
                           ;; Final callback when Enter is pressed
                           (lambda (search-string editor)
                             (let ((buffer (current-buffer editor)))
                               (when buffer
                                 (clear-isearch-state buffer)))
                             (setf (isearch-original-position editor) nil)
                             (format t "Isearch completed: ~A~%" search-string))
                           ;; Live callback as user types
                           #'live-isearch-forward))))

;; Register some basic commands
(register-command "quit" (lambda (editor) (format t "Quit command executed~%")))
(register-command "save-buffer" (lambda (editor) (format t "Save buffer command executed~%")))
(register-command "kill-buffer" (lambda (editor) (format t "Kill buffer command executed~%")))
(register-command "list-buffers" (lambda (editor) (format t "List buffers command executed~%")))

