(in-package :tle)

(defclass buffer ()
  ((id :initarg :id :reader buffer-id :initform (gensym "BUFFER-")))
  (:documentation "A buffer implementation"))

(defgeneric buffer-line-count (buffer)
  (:documentation "Get the number of lines the buffer holds"))

(defgeneric buffer-line (buffer line-number)
  (:documentation "Get a line from the buffer"))

(defgeneric buffer-insert (buffer content)
  (:documentation "Insert into a buffer"))

(defgeneric buffer-delete (buffer amount)
  (:documentation "Delete amount (number) from a buffer"))

(defgeneric buffer-undo (buffer)
  (:documentation "Undo a change to a buffer"))

(defgeneric buffer-redo (buffer)
  (:documentation "Redo a change to a buffer"))

(defgeneric buffer-set-point (buffer line column)
  (:documentation "Set the point (cursor) position in the buffer"))

(defgeneric buffer-get-point (buffer)
  (:documentation "Get the current point (cursor) position"))

(defgeneric buffer-set-mark (buffer line column)
  (:documentation "Set the mark position in the buffer"))

(defgeneric buffer-get-mark (buffer)
  (:documentation "Get the current mark position"))

(defgeneric buffer-clear-mark (buffer)
  (:documentation "Clear the mark"))

(defgeneric forward-char (buffer)
  (:documentation "Move point forward one character"))

(defgeneric backward-char (buffer)
  (:documentation "Move point backward one character"))

(defgeneric next-line (buffer)
  (:documentation "Move point to the next line"))

(defgeneric previous-line (buffer)
  (:documentation "Move point to the previous line"))

(defgeneric insert-char (buffer char)
  (:documentation "Insert a character at the current point position"))

(defgeneric insert-newline (buffer)
  (:documentation "Insert a newline at the current point position"))


(defclass standard-buffer (buffer)
  ((%lines :initform (make-array 0) :accessor lines)
   (%history :initform nil :accessor history)
   (%name :initarg :name :accessor buffer-name :initform "Untitled")
   (%point :initform (list 0 0) :accessor buffer-point)
   (%mark :initform nil :accessor buffer-mark)
   )
  (:documentation "A stardard buffer implementation"))

(defun make-standard-buffer (name)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line 1 with some text" "line 2 with more content" "line 3 final line"))
    (setf (buffer-name buf) name)
    (buffer-set-point buf 2 5)
    (buffer-set-mark buf 0 7)
    buf))

(defmethod buffer-line-count ((buffer standard-buffer))
  (length (lines buffer)))

(defmethod buffer-line ((buffer standard-buffer) line-number)
  (aref (lines buffer) line-number))

(defmethod buffer-insert ((buffer standard-buffer) content)
  )

(defmethod buffer-delete ((buffer standard-buffer) amount)
  )

(defmethod buffer-undo ((buffer standard-buffer))
  )

(defmethod buffer-redo ((buffer standard-buffer))
  )

(defmethod buffer-move-forward ((buffer standard-buffer) amount)
  )

(defmethod buffer-move-down ((buffer standard-buffer) amount)
  )

(defmethod buffer-set-point ((buffer standard-buffer) line column)
  (setf (buffer-point buffer) (list line column)))

(defmethod buffer-get-point ((buffer standard-buffer))
  (buffer-point buffer))

(defmethod buffer-set-mark ((buffer standard-buffer) line column)
  (setf (buffer-mark buffer) (list line column)))

(defmethod buffer-get-mark ((buffer standard-buffer))
  (buffer-mark buffer))

(defmethod buffer-clear-mark ((buffer standard-buffer))
  (setf (buffer-mark buffer) nil))

(defmethod forward-char ((buffer standard-buffer))
  "Move point forward one character, wrapping to next line if needed"
  (when (> (buffer-line-count buffer) 0)
    (let* ((point (buffer-get-point buffer))
           (line (first point))
           (col (second point))
           (current-line-text (buffer-line buffer line))
           (line-length (length current-line-text)))
      (cond
        ;; If at end of line and not at last line, move to beginning of next line
        ((and (>= col line-length) (< line (1- (buffer-line-count buffer))))
         (buffer-set-point buffer (1+ line) 0))
        ;; If within the line, move forward one character
        ((< col line-length)
         (buffer-set-point buffer line (1+ col)))
        ;; Otherwise, stay at current position (end of buffer)
        (t nil)))))

(defmethod backward-char ((buffer standard-buffer))
  "Move point backward one character, wrapping to previous line if needed"
  (when (> (buffer-line-count buffer) 0)
    (let* ((point (buffer-get-point buffer))
           (line (first point))
           (col (second point)))
      (cond
        ;; If at beginning of line and not at first line, move to end of previous line
        ((and (= col 0) (> line 0))
         (let ((prev-line-text (buffer-line buffer (1- line))))
           (buffer-set-point buffer (1- line) (length prev-line-text))))
        ;; If within the line, move backward one character
        ((> col 0)
         (buffer-set-point buffer line (1- col)))
        ;; Otherwise, stay at current position (beginning of buffer)
        (t nil)))))

(defmethod next-line ((buffer standard-buffer))
  "Move point to the next line, preserving column position when possible"
  (when (> (buffer-line-count buffer) 0)
    (let* ((point (buffer-get-point buffer))
           (line (first point))
           (col (second point)))
      (when (< line (1- (buffer-line-count buffer)))
        (let* ((next-line-text (buffer-line buffer (1+ line)))
               (next-line-length (length next-line-text))
               (new-col (min col next-line-length)))
          (buffer-set-point buffer (1+ line) new-col))))))

(defmethod previous-line ((buffer standard-buffer))
  "Move point to the previous line, preserving column position when possible"
  (when (> (buffer-line-count buffer) 0)
    (let* ((point (buffer-get-point buffer))
           (line (first point))
           (col (second point)))
      (when (> line 0)
        (let* ((prev-line-text (buffer-line buffer (1- line)))
               (prev-line-length (length prev-line-text))
               (new-col (min col prev-line-length)))
          (buffer-set-point buffer (1- line) new-col))))))

(defmethod insert-char ((buffer standard-buffer) char)
  "Insert a character at the current point position"
  (when (> (buffer-line-count buffer) 0)
    ;; Clear the mark before insertion
    (buffer-clear-mark buffer)
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (col (second point))
           (current-line (buffer-line buffer line-num))
           (new-line (concatenate 'string 
                                  (subseq current-line 0 col)
                                  (string char)
                                  (subseq current-line col))))
      ;; Update the line with the new character
      (setf (aref (lines buffer) line-num) new-line)
      ;; Move point forward by one character
      (buffer-set-point buffer line-num (1+ col)))))

(defmethod insert-newline ((buffer standard-buffer))
  "Insert a newline at the current point position, splitting the current line"
  (when (> (buffer-line-count buffer) 0)
    ;; Clear the mark before insertion
    (buffer-clear-mark buffer)
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (col (second point))
           (current-line (buffer-line buffer line-num))
           (left-part (subseq current-line 0 col))
           (right-part (subseq current-line col))
           (old-lines (lines buffer))
           (old-length (length old-lines))
           (new-lines (make-array (1+ old-length))))
      ;; Copy lines before the split point
      (loop for i from 0 below line-num do
        (setf (aref new-lines i) (aref old-lines i)))
      ;; Set the left part of the split line
      (setf (aref new-lines line-num) left-part)
      ;; Set the right part as the new line
      (setf (aref new-lines (1+ line-num)) right-part)
      ;; Copy lines after the split point
      (loop for i from (1+ line-num) below old-length do
        (setf (aref new-lines (1+ i)) (aref old-lines i)))
      ;; Update the buffer with new lines array
      (setf (lines buffer) new-lines)
      ;; Move point to the beginning of the new line
      (buffer-set-point buffer (1+ line-num) 0))))

(defun render-line-with-markers (line-text line-number point-line point-col mark-line mark-col)
  "Render a line with point cursor, mark indicators, and highlighting between them."
  (if (and point-line mark-line)
      (render-line-with-selection line-text line-number point-line point-col mark-line mark-col)
      (render-line-with-cursor-only line-text line-number point-line point-col mark-line mark-col)))

(defun render-line-with-cursor-only (line-text line-number point-line point-col mark-line mark-col)
  "Render a line with just cursor and mark, no selection highlighting."
  (render-line-with-spans line-text line-number nil nil nil nil point-line point-col mark-line mark-col))

(defun render-line-with-selection (line-text line-number point-line point-col mark-line mark-col)
  "Render a line with selection highlighting between mark and point."
  (let* ((selection-range (get-selection-range point-line point-col mark-line mark-col))
         (start-line (first selection-range))
         (start-col (second selection-range))
         (end-line (third selection-range))
         (end-col (fourth selection-range)))
    (cond
      ;; Line is before selection
      ((< line-number start-line)
       (render-line-with-cursor-only line-text line-number point-line point-col mark-line mark-col))
      ;; Line is after selection
      ((> line-number end-line)
       (render-line-with-cursor-only line-text line-number point-line point-col mark-line mark-col))
      ;; Line is within selection
      ((and (>= line-number start-line) (<= line-number end-line))
       (render-line-with-highlight line-text line-number start-line start-col end-line end-col point-line point-col mark-line mark-col))
      ;; Default case
      (t (render-line-with-cursor-only line-text line-number point-line point-col mark-line mark-col)))))

(defun get-selection-range (point-line point-col mark-line mark-col)
  "Get the normalized selection range (start-line start-col end-line end-col)."
  (if (or (< point-line mark-line)
          (and (= point-line mark-line) (< point-col mark-col)))
      (list point-line point-col mark-line mark-col)
      (list mark-line mark-col point-line point-col)))

(defun render-line-with-highlight (line-text line-number start-line start-col end-line end-col point-line point-col mark-line mark-col)
  "Render a line with highlighted selection and background-colored markers."
  (render-line-with-spans line-text line-number start-line start-col end-line end-col point-line point-col mark-line mark-col))

(defun render-line-with-spans (line-text line-number start-line start-col end-line end-col point-line point-col mark-line mark-col)
  "Render a line character by character with appropriate spans for selection, cursor, and mark."
  (let* ((result "")
         ;; Only calculate selection bounds if we have a valid selection
         (has-selection (and start-line end-line))
         (line-start-col (when has-selection
                           (if (= line-number start-line) start-col 0)))
         (line-end-col (when has-selection
                         (if (= line-number end-line) end-col (length line-text))))
         (cursor-pos (when (and point-line (= line-number point-line)) point-col))
         (mark-pos (when (and mark-line (= line-number mark-line)) mark-col)))
    
    (loop for i from 0 to (length line-text) do
      (let ((char (if (< i (length line-text)) 
                      (char line-text i) 
                      nil))
            (in-selection (and line-start-col line-end-col 
                               (>= i line-start-col) (< i line-end-col)))
            (is-cursor (and cursor-pos (= i cursor-pos)))
            (is-mark (and mark-pos (= i mark-pos))))
        
        (cond
          ;; Handle end of line positions (cursor/mark at end)
          ((and (= i (length line-text)) (or is-cursor is-mark))
           (setf result (concatenate 'string result
                                     (format nil "<span class=\"~A\">&nbsp;</span>"
                                             (cond (is-cursor "cursor")
                                                   (is-mark "mark"))))))
          ;; Handle characters with various combinations of states
          ((and char (or in-selection is-cursor is-mark))
           (let ((classes (remove nil (list (when in-selection "selection")
                                             (when is-cursor "cursor") 
                                             (when is-mark "mark")))))
             (setf result (concatenate 'string result
                                       (format nil "<span class=\"~{~A~^ ~}\">~A</span>"
                                               classes
                                               (escape-html-char char))))))
          ;; Regular character
          (char
           (setf result (concatenate 'string result (escape-html-char char)))))))
    result))

(defun escape-html-char (char)
  "Escape HTML special characters."
  (case char
    (#\< "&lt;")
    (#\> "&gt;")
    (#\& "&amp;")
    (#\" "&quot;")
    (t (string char))))


(defmethod render ((buffer standard-buffer) (ui ui-implementation))
  "Render a standard buffer with HTML line numbers, point cursor, and mark."
  (if (slot-boundp buffer '%lines)
      (let ((point (buffer-get-point buffer))
            (mark (buffer-get-mark buffer)))
        ;; Validate point structure
        (unless (and point (listp point) (>= (length point) 2) 
                     (numberp (first point)) (numberp (second point)))
          (error "Invalid point structure: ~A" point))
        ;; Validate mark structure if it exists
        (when (and mark (not (and (listp mark) (>= (length mark) 2) 
                                  (numberp (first mark)) (numberp (second mark)))))
          (error "Invalid mark structure: ~A" mark))
        
        (format nil "<div class=\"buffer-content\">~{~A~^~%~}</div>"
                (loop for i from 0 below (buffer-line-count buffer)
                      collect (let ((line-text (buffer-line buffer i))
                                    (point-line (first point))
                                    (point-col (second point))
                                    (mark-line (when mark (first mark)))
                                    (mark-col (when mark (second mark))))
                                (format nil "<div class=\"line\"><span class=\"line-number\">~3D</span><span class=\"line-content\">~A</span></div>"
                                        (1+ i)
                                        (render-line-with-markers line-text i point-line point-col mark-line mark-col))))))
      "<div class=\"buffer-content\">Empty buffer</div>"))
