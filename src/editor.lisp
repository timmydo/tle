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
   (%minibuffer-callback :accessor minibuffer-callback :initform nil)))

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

(defun activate-minibuffer (editor prompt &optional completion-function callback-function)
  "Activate the minibuffer with the given prompt, optional completion function, and callback function."
  (setf (minibuffer editor) (make-empty-buffer "*minibuffer*"))
  (setf (minibuffer-active-p editor) t)
  (setf (minibuffer-prompt editor) prompt)
  (setf (minibuffer-completion-function editor) completion-function)
  (setf (minibuffer-callback editor) callback-function))

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
    contents))

(defun get-minibuffer-contents (editor)
  "Get the current contents of the minibuffer."
  (if (and (minibuffer editor) (minibuffer-active-p editor))
      (buffer-line (minibuffer editor) 0)
      ""))

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
       (deactivate-minibuffer editor)
       (format t "Minibuffer cancelled~%")
       t)
      
      ;; Backspace - delete character in minibuffer
      ((string= key "Backspace")
       (delete-backward-char minibuf)
       t)
      
      ;; Delete - delete character at point in minibuffer
      ((string= key "Delete")
       (delete-char minibuf)
       t)
      
      ;; Regular characters - insert into minibuffer
      ((and (= (length key) 1)
            (not ctrl)
            (not alt)
            (graphic-char-p (char key 0)))
       (insert-char minibuf (char key 0))
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

;; Register some basic commands
(register-command "quit" (lambda (editor) (format t "Quit command executed~%")))
(register-command "save-buffer" (lambda (editor) (format t "Save buffer command executed~%")))
(register-command "kill-buffer" (lambda (editor) (format t "Kill buffer command executed~%")))
(register-command "list-buffers" (lambda (editor) (format t "List buffers command executed~%")))

