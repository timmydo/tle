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
   (%isearch-original-position :accessor isearch-original-position :initform nil)
   (%query-replace-from-string :accessor query-replace-from-string :initform nil)
   (%query-replace-to-string :accessor query-replace-to-string :initform nil)
   (%query-replace-active :accessor query-replace-active-p :initform nil)
   (%query-replace-matches :accessor query-replace-matches :initform nil)
   (%query-replace-current-match :accessor query-replace-current-match :initform 0)
   (%query-replace-replacements :accessor query-replace-replacements :initform nil)
   (%query-replace-original-position :accessor query-replace-original-position :initform nil)
   (%query-replace-original-content :accessor query-replace-original-content :initform nil)
   (%query-replace-old-recording :accessor query-replace-old-recording :initform nil)
   (%in-recursive-edit :accessor in-recursive-edit-p :initform nil)))

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
  (format t "DEBUG: activate-minibuffer called with prompt '~A', callback ~A~%" prompt callback-function)
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

(defun handle-minibuffer-input (editor key ctrl alt shift &optional meta)
  "Handle input when minibuffer is active."
  (let ((minibuf (minibuffer editor)))
    (cond
      ;; Special handling for query-replace single-key commands
      ((and (query-replace-active-p editor)
            (eq (minibuffer-callback editor) 'query-replace-action-callback)
            (> (length key) 0)
            (not ctrl) (not alt) (not shift)
            (member key '("y" "n" "q" "!" "." "," "^" "u" "U" "e" "E" "r" "w" "l" "?" " ") :test #'string=))
       (deactivate-minibuffer editor)
       (query-replace-action-callback key editor)
       t)
      
      ;; Special handling for Delete key in query-replace (same as 'n')
      ((and (query-replace-active-p editor)
            (eq (minibuffer-callback editor) 'query-replace-action-callback)
            (string= key "Delete"))
       (deactivate-minibuffer editor)
       (query-replace-action-callback "n" editor)
       t)
      
      ;; Enter key - execute callback or command
      ((string= key "Enter")
       (let ((contents (get-minibuffer-contents editor))
             (callback (minibuffer-callback editor)))
         (format t "DEBUG: Enter pressed in minibuffer. Contents: '~A', Callback: ~A~%" 
                 contents callback)
         (deactivate-minibuffer editor)
         (if callback
             (progn
               (format t "DEBUG: Calling callback ~A with contents '~A'~%" callback contents)
               (funcall callback contents editor))
             (progn
               (format t "DEBUG: Executing named command '~A'~%" contents)
               (execute-named-command contents editor))))
       t)
      
      ;; C-M-c - exit recursive edit
      ((and ctrl alt (string= key "c"))
       (when (in-recursive-edit-p editor)
         (exit-recursive-edit "" editor))
       t)
      
      ;; Escape key or C-g - cancel minibuffer
      ((or (string= key "Escape")
           (and ctrl (string= key "g")))
       ;; If this is query-replace, restore original position and clean up
       (when (query-replace-active-p editor)
         (let ((original-pos (query-replace-original-position editor))
               (buffer (current-buffer editor)))
           (when (and original-pos buffer)
             (buffer-set-point buffer (first original-pos) (second original-pos)))
           (clear-query-replace-state editor)
           (format t "Query replace cancelled~%")))
       
       ;; If this is an isearch, restore original position
       (let ((original-pos (isearch-original-position editor))
             (buffer (current-buffer editor)))
         (when (and original-pos buffer)
           (clear-isearch-state buffer)
           (buffer-set-point buffer (first original-pos) (second original-pos))
           (setf (isearch-original-position editor) nil)
           (format t "Isearch cancelled, position restored~%")))
       
       (deactivate-minibuffer editor)
       (unless (query-replace-active-p editor)
         (format t "Minibuffer cancelled~%"))
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

(defun save-buffer-command (editor)
  "Save the current buffer to its file."
  (let ((buffer (current-buffer editor)))
    (when buffer
      (if (save-buffer buffer)
          (format t "Buffer saved successfully~%")
          (format t "Failed to save buffer~%")))))

(defun query-replace-from-command (from-string editor)
  "First step of query-replace - get the 'from' string and prompt for 'to' string."
  (let ((trimmed-from (string-trim " " from-string)))
    (format t "DEBUG: query-replace-from-command called with '~A', trimmed to '~A'~%" from-string trimmed-from)
    ;; Store the from-string in a temporary slot and prompt for to-string
    (setf (query-replace-from-string editor) trimmed-from)
    (activate-minibuffer editor 
                         (format nil "Replace '~A' with: " trimmed-from)
                         nil 
                         'query-replace-to-command)))

(defun query-replace-to-command (to-string editor)
  "Second step of query-replace - start interactive replacement."
  (let ((buffer (current-buffer editor))
        (from-string (query-replace-from-string editor))
        (trimmed-to (string-trim " " to-string)))
    (format t "DEBUG: query-replace-to-command called with to-string '~A', trimmed to '~A'~%" to-string trimmed-to)
    (format t "DEBUG: from-string is '~A', buffer is ~A~%" from-string buffer)
    (when buffer
      (format t "DEBUG: Starting interactive query replace from '~A' to '~A'~%" from-string trimmed-to)
      (start-interactive-query-replace editor from-string trimmed-to))
    ;; Clear the stored from-string
    (setf (query-replace-from-string editor) nil)))

(defun start-query-replace (editor)
  "Start query-replace by prompting for the search string."
  (activate-minibuffer editor "Query replace: " nil 'query-replace-from-command))

(defun start-interactive-query-replace (editor from-string to-string)
  "Start interactive query-replace session."
  (let ((buffer (current-buffer editor)))
    (when (and buffer from-string to-string 
               (> (length from-string) 0))
      ;; Store original position and content for undo
      (setf (query-replace-original-position editor) (buffer-get-point buffer))
      (setf (query-replace-original-content editor) (copy-seq (lines buffer)))
      
      ;; Disable undo recording during the session
      (setf (query-replace-old-recording editor) (buffer-recording-undo-p buffer))
      (setf (buffer-recording-undo-p buffer) nil)
      
      ;; Check if region is active for feedback
      (let ((mark (buffer-get-mark buffer))
            (region-message (if (buffer-get-mark buffer) " in region" "")))
        ;; Find all matches from cursor position (respecting region if active)
        (let ((matches (find-matches-from-point buffer from-string)))
          (if matches
              (progn
                ;; Initialize query-replace state
                (setf (query-replace-active-p editor) t)
                (setf (query-replace-from-string editor) from-string)
                (setf (query-replace-to-string editor) to-string)
                (setf (query-replace-matches editor) matches)
                (setf (query-replace-current-match editor) 0)
                (setf (query-replace-replacements editor) '())
                
                ;; Move to first match and start interactive session
                (let ((first-match (first matches)))
                  (buffer-set-point buffer (first first-match) (second first-match)))
                
                ;; Show info about matches found
                (format t "Found ~A occurrence~:P of '~A'~A~%" 
                        (length matches) from-string region-message)
                
                ;; Activate minibuffer for interactive replacement
                (query-replace-prompt-for-action editor))
              (progn
                (format t "No matches found for '~A'~A~%" from-string region-message)
                (clear-query-replace-state editor))))))))

(defun query-replace-prompt-for-action (editor)
  "Prompt user for action on current match."
  (let* ((buffer (current-buffer editor))
         (from-string (or (query-replace-from-string editor) ""))
         (to-string (query-replace-to-string editor))
         (current-match-idx (query-replace-current-match editor))
         (matches (query-replace-matches editor))
         (current-match (nth current-match-idx matches)))
    
    (when current-match
      ;; Highlight current match by positioning cursor
      (buffer-set-point buffer (first current-match) (second current-match))
      
      ;; Show replacement prompt
      (let ((prompt (format nil "Replace '~A' with '~A'? (y/n/!/q/^/? for help): " 
                            from-string to-string)))
        (activate-minibuffer editor prompt nil 'query-replace-action-callback)))))

(defun query-replace-action-callback (action editor)
  "Handle user's action in interactive query-replace."
  (let ((action-char (if (> (length action) 0) (char action 0) #\q)))
    (case action-char
      ((#\y #\Space #\Return)  ; yes, replace this match (Enter acts like 'y')
       (perform-current-replacement editor)
       (advance-to-next-match editor))
      ((#\n #\Delete) ; no, skip this match  
       (advance-to-next-match editor))
      (#\!            ; replace all remaining matches
       (replace-all-remaining-matches editor))
      (#\q            ; quit
       (finish-query-replace editor))
      (#\.            ; replace and quit
       (perform-current-replacement editor)
       (finish-query-replace editor))
      (#\,            ; replace but don't advance
       (perform-current-replacement editor)
       (query-replace-prompt-for-action editor))
      (#\^            ; go back to previous match
       (go-to-previous-match editor))
      (#\u            ; undo last replacement
       (undo-last-replacement editor))
      (#\U            ; undo all replacements
       (undo-all-replacements editor))
      (#\e            ; edit replacement string
       (edit-replacement-string editor))
      (#\E            ; edit replacement string with exact case
       (edit-replacement-string editor t))
      (#\r            ; recursive edit (simplified)
       (start-recursive-edit editor))
      (#\w            ; delete match and start recursive edit  
       (perform-current-replacement editor)
       (start-recursive-edit editor))
      (#\l            ; clear screen and redisplay
       (format t "Screen cleared~%")
       (query-replace-prompt-for-action editor))
      (#\?            ; show help
       (show-query-replace-help editor))
      (otherwise      ; invalid input
       (format t "Invalid action '~A'. Type ? for help~%" action)
       (query-replace-prompt-for-action editor)))))

(defun clear-query-replace-state (editor)
  "Clear query-replace state."
  ;; Restore undo recording state if it was modified
  (when (query-replace-old-recording editor)
    (let ((buffer (current-buffer editor)))
      (when buffer
        (setf (buffer-recording-undo-p buffer) (query-replace-old-recording editor)))))
  
  (setf (query-replace-active-p editor) nil)
  (setf (query-replace-to-string editor) nil)
  (setf (query-replace-matches editor) nil)
  (setf (query-replace-current-match editor) 0)
  (setf (query-replace-replacements editor) nil)
  (setf (query-replace-original-position editor) nil)
  (setf (query-replace-original-content editor) nil)
  (setf (query-replace-old-recording editor) nil))

(defun perform-current-replacement (editor)
  "Replace the current match."
  (let* ((buffer (current-buffer editor))
         (from-string (query-replace-from-string editor))
         (to-string (query-replace-to-string editor))
         (current-match-idx (query-replace-current-match editor))
         (matches (query-replace-matches editor))
         (current-match (nth current-match-idx matches)))
    
    (when current-match
      (let ((line (first current-match))
            (col (second current-match))
            (from-len (length from-string)))
        
        ;; Record this replacement for undo
        (push (list :line line :col col :from-text from-string :to-text to-string
                    :original-text (buffer-line buffer line))
              (query-replace-replacements editor))
        
        ;; Perform replacement
        (buffer-set-point buffer line col)
        (buffer-set-mark buffer line (+ col from-len))
        (delete-region buffer)
        (buffer-clear-mark buffer)
        (buffer-insert buffer to-string)
        
        ;; Update matches list to adjust positions after replacement
        (update-matches-after-replacement editor line col from-len (length to-string))
        
        (format t "Replaced '~A' with '~A' at line ~A, column ~A~%" 
                from-string to-string (1+ line) (1+ col))))))

(defun advance-to-next-match (editor)
  "Advance to the next match in query-replace session."
  (let* ((current-idx (query-replace-current-match editor))
         (matches (query-replace-matches editor))
         (next-idx (1+ current-idx)))
    
    (if (< next-idx (length matches))
        (progn
          (setf (query-replace-current-match editor) next-idx)
          (query-replace-prompt-for-action editor))
        (finish-query-replace editor))))

(defun replace-all-remaining-matches (editor)
  "Replace all remaining matches without prompting."
  (let* ((current-idx (query-replace-current-match editor))
         (matches (query-replace-matches editor))
         (remaining-count (- (length matches) current-idx)))
    
    (loop for i from current-idx below (length matches) do
      (perform-current-replacement editor))
    
    (format t "Replaced ~A remaining occurrence~:P~%" remaining-count)
    (finish-query-replace editor)))

(defun finish-query-replace (editor)
  "Finish query-replace session and show summary."
  (let* ((buffer (current-buffer editor))
         (replacement-count (length (query-replace-replacements editor)))
         (original-content (query-replace-original-content editor))
         (original-position (query-replace-original-position editor))
         (old-recording (query-replace-old-recording editor)))
    
    ;; If replacements were made, create a single undo record
    (when (and (> replacement-count 0) original-content original-position)
      ;; Re-enable undo recording to add our comprehensive record
      (setf (buffer-recording-undo-p buffer) old-recording)
      
      ;; Add single undo record for all replacements (similar to batch mode)
      (add-undo-record buffer :query-replace-interactive original-position
                       (list :original-content original-content
                             :new-content (copy-seq (lines buffer))
                             :from-string (query-replace-from-string editor)
                             :to-string (query-replace-to-string editor)
                             :replacements replacement-count)))
    
    ;; If no replacements were made, just restore undo recording
    (when (and (= replacement-count 0) old-recording)
      (setf (buffer-recording-undo-p buffer) old-recording))
    
    (format t "Query replace finished. Made ~A replacement~:P~%" replacement-count)
    (clear-query-replace-state editor)))

(defun go-to-previous-match (editor)
  "Go back to previous match in query-replace session."
  (let* ((current-idx (query-replace-current-match editor))
         (prev-idx (1- current-idx)))
    
    (if (>= prev-idx 0)
        (progn
          (setf (query-replace-current-match editor) prev-idx)
          (query-replace-prompt-for-action editor))
        (progn
          (format t "No previous match~%")
          (query-replace-prompt-for-action editor)))))

(defun undo-last-replacement (editor)
  "Undo the last replacement made."
  (let ((replacements (query-replace-replacements editor)))
    (if replacements
        (let* ((last-replacement (first replacements))
               (line (getf last-replacement :line))
               (col (getf last-replacement :col))
               (original-text (getf last-replacement :original-text))
               (buffer (current-buffer editor)))
          
          ;; Restore original line
          (setf (nth line (lines buffer)) original-text)
          
          ;; Remove from replacements list
          (setf (query-replace-replacements editor) (rest replacements))
          
          (format t "Undid replacement at line ~A~%" (1+ line))
          (query-replace-prompt-for-action editor))
        (progn
          (format t "No replacements to undo~%")
          (query-replace-prompt-for-action editor)))))

(defun undo-all-replacements (editor)
  "Undo all replacements made in this session."
  (let ((replacement-count (length (query-replace-replacements editor))))
    (loop while (query-replace-replacements editor) do
      (undo-last-replacement-without-prompt editor))
    
    (format t "Undid ~A replacement~:P~%" replacement-count)
    (query-replace-prompt-for-action editor)))

(defun undo-last-replacement-without-prompt (editor)
  "Undo last replacement without re-prompting."
  (let ((replacements (query-replace-replacements editor)))
    (when replacements
      (let* ((last-replacement (first replacements))
             (line (getf last-replacement :line))
             (original-text (getf last-replacement :original-text))
             (buffer (current-buffer editor)))
        
        ;; Restore original line
        (setf (nth line (lines buffer)) original-text)
        
        ;; Remove from replacements list
        (setf (query-replace-replacements editor) (rest replacements))))))

(defun edit-replacement-string (editor &optional exact-case)
  "Allow user to edit the replacement string."
  (declare (ignore exact-case)) ; TODO: implement exact case handling
  (let ((current-to (query-replace-to-string editor)))
    (activate-minibuffer editor 
                         (format nil "Edit replacement string (was '~A'): " current-to)
                         nil
                         'edit-replacement-callback)))

(defun edit-replacement-callback (new-string editor)
  "Handle edited replacement string."
  (setf (query-replace-to-string editor) (string-trim " " new-string))
  (format t "Replacement string updated to: '~A'~%" (query-replace-to-string editor))
  (query-replace-prompt-for-action editor))

(defun update-matches-after-replacement (editor line col from-len to-len)
  "Update match positions after a replacement changes text length."
  (let ((matches (query-replace-matches editor))
        (length-diff (- to-len from-len)))
    
    (when (/= length-diff 0)
      ;; Update positions of matches on same line that come after the replacement
      (loop for match in matches
            for i from 0
            when (and (= (first match) line)
                      (> (second match) col))
            do (setf (second match) (+ (second match) length-diff)))
      
      ;; Store updated matches
      (setf (query-replace-matches editor) matches))))

(defun show-query-replace-help (editor)
  "Show help for query-replace interactive commands."
  (format t "~%Query Replace Help:~%")
  (format t "  y, SPC or RET - replace this match and move to next~%")
  (format t "  n or Delete   - skip this match and move to next~%")
  (format t "  q             - quit query-replace~%")
  (format t "  !             - replace all remaining matches without asking~%")
  (format t "  .             - replace this match and quit~%")
  (format t "  ,             - replace this match but don't move to next~%")
  (format t "  ^             - move back to previous match~%")
  (format t "  u             - undo last replacement~%")
  (format t "  U             - undo all replacements~%")
  (format t "  e             - edit replacement string~%")
  (format t "  E             - edit replacement string with exact case~%")
  (format t "  r             - enter recursive edit~%")
  (format t "  w             - replace match and enter recursive edit~%")
  (format t "  l             - clear screen and redisplay~%")
  (format t "  ?             - show this help~%")
  (format t "~%Press any key to continue...~%")
  
  ;; Wait for user to press a key, then continue
  (activate-minibuffer editor "Press any key to continue: " nil 'query-replace-help-continue))

(defun query-replace-help-continue (input editor)
  "Continue query-replace after showing help."
  (declare (ignore input))
  (query-replace-prompt-for-action editor))

(defun start-recursive-edit (editor)
  "Start a recursive edit during query-replace."
  (format t "~%Entering recursive edit. Press C-M-c when done to continue query-replace.~%")
  (format t "You can edit the buffer normally. The query-replace session will resume when you exit.~%")
  
  ;; Set up recursive edit state
  (setf (in-recursive-edit-p editor) t)
  
  ;; Prompt for exit
  (activate-minibuffer editor "Recursive edit (C-M-c to exit): " nil 'exit-recursive-edit))

(defun exit-recursive-edit (input editor)
  "Exit recursive edit and return to query-replace."
  (declare (ignore input))
  (setf (in-recursive-edit-p editor) nil)
  (format t "Exiting recursive edit, resuming query-replace~%")
  (query-replace-prompt-for-action editor))

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

(defun live-isearch-backward (search-string editor)
  "Live incremental search backward - called as user types in minibuffer."
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
              (if (isearch-backward buffer trimmed-string)
                  (format t "Isearch backward: Found '~A'~%" trimmed-string)
                  (format t "Isearch backward: '~A' not found~%" trimmed-string)))
            ;; Empty search string - restore original position and clear highlights
            (let ((original-pos (isearch-original-position editor)))
              (clear-isearch-state buffer)
              (when original-pos
                (buffer-set-point buffer (first original-pos) (second original-pos)))))))))

(defun start-isearch-backward (editor)
  "Start incremental search backward with live updates."
  (let ((buffer (current-buffer editor)))
    (when buffer
      ;; Store original cursor position
      (setf (isearch-original-position editor) (copy-list (buffer-get-point buffer)))
      ;; Activate minibuffer with live callback
      (activate-minibuffer editor "I-search backward: " nil 
                           ;; Final callback when Enter is pressed
                           (lambda (search-string editor)
                             (let ((buffer (current-buffer editor)))
                               (when buffer
                                 (clear-isearch-state buffer)))
                             (setf (isearch-original-position editor) nil)
                             (format t "Isearch backward completed: ~A~%" search-string))
                           ;; Live callback as user types
                           #'live-isearch-backward))))

;; Register some basic commands
(register-command "quit" (lambda (editor) (format t "Quit command executed~%")))
(register-command "save-buffer" (lambda (editor) (save-buffer-command editor)))
(register-command "save-buffer-as" 
  (lambda (editor) 
    (activate-minibuffer editor "Save buffer as: " nil 
                         (lambda (file-path editor)
                           (let ((buffer (current-buffer editor)))
                             (when buffer
                               (save-buffer-as buffer (string-trim " " file-path))))))))
(register-command "kill-buffer" (lambda (editor) (format t "Kill buffer command executed~%")))
(register-command "list-buffers" (lambda (editor) (format t "List buffers command executed~%")))

