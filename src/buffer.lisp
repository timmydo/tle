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
  (let ((result "")
        (line-start-col (if (= line-number start-line) start-col 0))
        (line-end-col (if (= line-number end-line) end-col (length line-text)))
        (cursor-pos (when (and point-line (= line-number point-line)) point-col))
        (mark-pos (when (and mark-line (= line-number mark-line)) mark-col)))
    
    (loop for i from 0 to (length line-text) do
      (let ((char (if (< i (length line-text)) 
                      (char line-text i) 
                      nil))
            (in-selection (and (>= i line-start-col) (< i line-end-col)))
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
