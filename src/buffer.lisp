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

(defgeneric delete-char (buffer)
  (:documentation "Delete character at point"))

(defgeneric delete-backward-char (buffer)
  (:documentation "Delete character before point"))

(defgeneric kill-line (buffer)
  (:documentation "Delete from point to end of line"))

(defgeneric kill-whole-line (buffer)
  (:documentation "Delete entire current line"))

(defgeneric kill-word (buffer)
  (:documentation "Delete word forward"))


;; Undo tree data structures
(defclass undo-record ()
  ((operation :initarg :operation :reader undo-operation)
   (position :initarg :position :reader undo-position)
   (data :initarg :data :reader undo-data)
   (timestamp :initform (get-universal-time) :reader undo-timestamp))
  (:documentation "A single undo operation record"))

(defclass undo-tree-node ()
  ((record :initarg :record :reader node-record :initform nil)
   (parent :initarg :parent :accessor node-parent :initform nil)
   (children :initform '() :accessor node-children)
   (branch-point :initform nil :accessor node-branch-point))
  (:documentation "A node in the undo tree"))

(defclass undo-tree ()
  ((root :initform (make-instance 'undo-tree-node) :reader tree-root)
   (current :accessor tree-current)
   (size :initform 0 :accessor tree-size)
   (max-size :initform 1000 :accessor tree-max-size))
  (:documentation "Undo tree with branching history like Emacs"))

(defmethod initialize-instance :after ((tree undo-tree) &key)
  (setf (tree-current tree) (tree-root tree)))

(defclass standard-buffer (buffer)
  ((%lines :initform (make-array 0) :accessor lines)
   (%undo-tree :initform (make-instance 'undo-tree) :accessor buffer-undo-tree)
   (%name :initarg :name :accessor buffer-name :initform "Untitled")
   (%point :initform (list 0 0) :accessor buffer-point)
   (%mark :initform nil :accessor buffer-mark)
   (%recording-undo :initform t :accessor buffer-recording-undo-p))
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

;; Undo tree operations
(defun add-undo-record (buffer operation position data)
  "Add a new undo record to the buffer's undo tree"
  (when (buffer-recording-undo-p buffer)
    (let* ((tree (buffer-undo-tree buffer))
           (record (make-instance 'undo-record 
                                  :operation operation 
                                  :position position 
                                  :data data))
           (new-node (make-instance 'undo-tree-node 
                                    :record record 
                                    :parent (tree-current tree))))
      ;; Add as child to current node
      (push new-node (node-children (tree-current tree)))
      ;; Move current pointer to new node
      (setf (tree-current tree) new-node)
      ;; Update tree size
      (incf (tree-size tree))
      ;; Trim tree if it exceeds max size
      (trim-undo-tree tree))))

(defun trim-undo-tree (tree)
  "Remove oldest entries if tree exceeds max size"
  (when (> (tree-size tree) (tree-max-size tree))
    ;; Simple implementation: just reset if too large
    ;; In practice, you'd want a more sophisticated pruning algorithm
    (when (> (tree-size tree) (* 1.5 (tree-max-size tree)))
      (setf (tree-current tree) (tree-root tree)
            (node-children (tree-root tree)) '()
            (tree-size tree) 0))))

(defun apply-undo-record (buffer record reverse-p)
  "Apply an undo record to the buffer, optionally in reverse"
  (let ((operation (undo-operation record))
        (position (undo-position record))
        (data (undo-data record)))
    (case operation
      (:insert-char
       (if reverse-p
           ;; Undo insert: delete the character and stay at original position
           (progn
             (buffer-set-point buffer (first position) (second position))
             (delete-char-without-undo buffer))
           ;; Redo insert: insert the character and move forward
           (progn
             (buffer-set-point buffer (first position) (second position))
             (insert-char-without-undo buffer data))))
      (:delete-char
       (if reverse-p
           ;; Undo delete: insert the character back and restore position
           (progn
             (buffer-set-point buffer (first position) (second position))
             (if (char= data #\Newline)
                 ;; Special case for newline: insert newline and adjust position
                 (progn
                   (insert-newline-without-undo buffer)
                   (buffer-set-point buffer (first position) (second position)))
                 ;; Normal character: insert and keep position before the char
                 (progn
                   (insert-char-without-undo buffer data)
                   (buffer-set-point buffer (first position) (second position)))))
           ;; Redo delete: delete the character again
           (progn
             (buffer-set-point buffer (first position) (second position))
             (delete-char-without-undo buffer))))
      (:delete-backward-char
         (if reverse-p
             ;; Undo delete-backward-char: insert the character back and restore original position
             (progn
               (if (char= data #\Newline)
                   ;; Special case for newline: restore split at original position
                   (let ((line-num (first position))
                         (col (second position)))
                     ;; The newline was at the end of the previous line
                     (when (> line-num 0)
                       (let ((prev-line-len (length (buffer-line buffer (1- line-num)))))
                         (buffer-set-point buffer (1- line-num) prev-line-len)
                         (insert-newline-without-undo buffer)
                         ;; Restore cursor to original position (beginning of new line)
                         (buffer-set-point buffer line-num col))))
                   ;; Normal character: insert at deletion point and restore cursor after it
                   (let ((line-num (first position))
                         (col (second position)))
                     (when (> col 0)
                       (buffer-set-point buffer line-num (1- col))
                       (insert-char-without-undo buffer data)
                       ;; Restore cursor to original position (after the character)
                       (buffer-set-point buffer line-num col)))))
             ;; Redo delete-backward-char: perform the deletion again
             (progn
               (buffer-set-point buffer (first position) (second position))
               (delete-backward-char-without-undo buffer))))
      (:insert-newline
       (if reverse-p
           ;; Undo newline: join lines
           (progn
             (buffer-set-point buffer (first position) (second position))
             (delete-char-without-undo buffer))
           ;; Redo newline: split line
           (progn
             (buffer-set-point buffer (first position) (second position))
             (insert-newline-without-undo buffer))))
      (:kill-line
       (if reverse-p
           ;; Undo kill-line: insert the killed text back at the original position
           (progn
             (buffer-set-point buffer (first position) (second position))
             (insert-text-without-undo buffer data)
             ;; Restore the original point position after insertion
             (buffer-set-point buffer (first position) (second position)))
           ;; Redo kill-line: delete from point to end of line again
           (progn
             (buffer-set-point buffer (first position) (second position))
             (kill-line-without-undo buffer))))
      (:kill-whole-line
       (if reverse-p
           ;; Undo kill-whole-line: insert the killed line back at the original position
           (progn
             (buffer-set-point buffer (first position) (second position))
             (insert-whole-line-without-undo buffer data)
             ;; Restore the original point position after insertion
             (buffer-set-point buffer (first position) (second position)))
           ;; Redo kill-whole-line: delete entire line again
           (progn
             (buffer-set-point buffer (first position) (second position))
             (kill-whole-line-without-undo buffer))))
      (:kill-word
       (if reverse-p
           ;; Undo kill-word: insert the killed text back at the original position
           (progn
             (buffer-set-point buffer (first position) (second position))
             (insert-text-without-undo buffer data)
             ;; Restore the original point position after insertion
             (buffer-set-point buffer (first position) (second position)))
           ;; Redo kill-word: delete word forward again
           (progn
             (buffer-set-point buffer (first position) (second position))
             (kill-word-without-undo buffer)))))))

(defmethod buffer-undo ((buffer standard-buffer))
  "Undo the last operation"
  (let* ((tree (buffer-undo-tree buffer))
         (current (tree-current tree)))
    (when (and current (node-record current))
      ;; Apply the current record in reverse
      (let ((old-recording (buffer-recording-undo-p buffer)))
        (setf (buffer-recording-undo-p buffer) nil)
        (apply-undo-record buffer (node-record current) t)
        (setf (buffer-recording-undo-p buffer) old-recording))
      ;; Move to parent node
      (when (node-parent current)
        (setf (tree-current tree) (node-parent current)))
      t)))

(defmethod buffer-redo ((buffer standard-buffer))
  "Redo the next operation"
  (let* ((tree (buffer-undo-tree buffer))
         (current (tree-current tree))
         (children (node-children current)))
    (when children
      ;; Choose the first child (in practice, you might want UI to choose branch)
      (let* ((next-node (first children))
             (old-recording (buffer-recording-undo-p buffer)))
        ;; Apply the record normally
        (setf (buffer-recording-undo-p buffer) nil)
        (apply-undo-record buffer (node-record next-node) nil)
        (setf (buffer-recording-undo-p buffer) old-recording)
        ;; Move to the child node
        (setf (tree-current tree) next-node)
        t))))

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

(defun insert-char-without-undo (buffer char)
  "Insert a character without recording undo information"
  (when (> (buffer-line-count buffer) 0)
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

(defmethod insert-char ((buffer standard-buffer) char)
  "Insert a character at the current point position"
  (when (> (buffer-line-count buffer) 0)
    ;; Clear the mark before insertion
    (buffer-clear-mark buffer)
    (let ((point (buffer-get-point buffer)))
      ;; Record undo information
      (add-undo-record buffer :insert-char (copy-list point) char)
      ;; Perform the insertion
      (insert-char-without-undo buffer char))))

(defun insert-newline-without-undo (buffer)
  "Insert a newline without recording undo information"
  (when (> (buffer-line-count buffer) 0)
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

(defmethod insert-newline ((buffer standard-buffer))
  "Insert a newline at the current point position, splitting the current line"
  (when (> (buffer-line-count buffer) 0)
    ;; Clear the mark before insertion
    (buffer-clear-mark buffer)
    (let ((point (buffer-get-point buffer)))
      ;; Record undo information
      (add-undo-record buffer :insert-newline (copy-list point) nil)
      ;; Perform the insertion
      (insert-newline-without-undo buffer))))

(defun delete-char-without-undo (buffer)
  "Delete character at point without recording undo information"
  (when (> (buffer-line-count buffer) 0)
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (col (second point))
           (current-line (buffer-line buffer line-num))
           (line-length (length current-line)))
      (cond
        ;; If point is within the line, delete the character at point
        ((< col line-length)
         (let ((new-line (concatenate 'string
                                      (subseq current-line 0 col)
                                      (subseq current-line (1+ col)))))
           (setf (aref (lines buffer) line-num) new-line)))
        ;; If point is at end of line and not at last line, join with next line
        ((and (>= col line-length) (< line-num (1- (buffer-line-count buffer))))
         (let* ((next-line (buffer-line buffer (1+ line-num)))
                (joined-line (concatenate 'string current-line next-line))
                (old-lines (lines buffer))
                (old-length (length old-lines))
                (new-lines (make-array (1- old-length))))
           ;; Copy lines before the join point
           (loop for i from 0 below line-num do
             (setf (aref new-lines i) (aref old-lines i)))
           ;; Set the joined line
           (setf (aref new-lines line-num) joined-line)
           ;; Copy lines after the deleted line
           (loop for i from (1+ line-num) below (1- old-length) do
             (setf (aref new-lines i) (aref old-lines (1+ i))))
           ;; Update the buffer with new lines array
           (setf (lines buffer) new-lines)))
        ;; Otherwise, at end of buffer, do nothing
        (t nil)))))

(defun delete-backward-char-without-undo (buffer)
  "Delete character before point without recording undo information"
  (when (> (buffer-line-count buffer) 0)
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (col (second point)))
      (cond
        ;; If point is within the line (not at beginning), delete the character before point
        ((> col 0)
         (let* ((current-line (buffer-line buffer line-num))
                (new-line (concatenate 'string
                                       (subseq current-line 0 (1- col))
                                       (subseq current-line col))))
           (setf (aref (lines buffer) line-num) new-line)
           ;; Move point backward by one character
           (buffer-set-point buffer line-num (1- col))))
        ;; If point is at beginning of line and not at first line, join with previous line
        ((and (= col 0) (> line-num 0))
         (let* ((prev-line (buffer-line buffer (1- line-num)))
                (current-line (buffer-line buffer line-num))
                (joined-line (concatenate 'string prev-line current-line))
                (old-lines (lines buffer))
                (old-length (length old-lines))
                (new-lines (make-array (1- old-length)))
                (new-col (length prev-line)))
           ;; Copy lines before the join point
           (loop for i from 0 below (1- line-num) do
             (setf (aref new-lines i) (aref old-lines i)))
           ;; Set the joined line
           (setf (aref new-lines (1- line-num)) joined-line)
           ;; Copy lines after the current line
           (loop for i from (1+ line-num) below old-length do
             (setf (aref new-lines (1- i)) (aref old-lines i)))
           ;; Update the buffer with new lines array
           (setf (lines buffer) new-lines)
           ;; Move point to end of previous line
           (buffer-set-point buffer (1- line-num) new-col)))
        ;; Otherwise, at beginning of buffer, do nothing
        (t nil)))))

(defmethod delete-char ((buffer standard-buffer))
  "Delete character at point, joining lines if at end of line"
  (when (> (buffer-line-count buffer) 0)
    ;; Clear the mark before deletion
    (buffer-clear-mark buffer)
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (col (second point))
           (current-line (buffer-line buffer line-num))
           (line-length (length current-line))
           (deleted-char nil))
      (cond
        ;; If point is within the line, record the character being deleted
        ((< col line-length)
         (setf deleted-char (char current-line col))
         (add-undo-record buffer :delete-char (copy-list point) deleted-char)
         (delete-char-without-undo buffer))
        ;; If point is at end of line and not at last line, record newline deletion
        ((and (>= col line-length) (< line-num (1- (buffer-line-count buffer))))
         (add-undo-record buffer :delete-char (copy-list point) #\Newline)
         (delete-char-without-undo buffer))
        ;; Otherwise, at end of buffer, do nothing
        (t nil)))))

(defmethod delete-backward-char ((buffer standard-buffer))
  "Delete character before point (backspace), joining lines if at beginning of line"
  (when (> (buffer-line-count buffer) 0)
    ;; Clear the mark before deletion
    (buffer-clear-mark buffer)
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (col (second point))
           (deleted-char nil))
      (cond
        ;; If point is within the line (not at beginning), record the character being deleted
        ((> col 0)
         (let ((current-line (buffer-line buffer line-num)))
           (setf deleted-char (char current-line (1- col)))
           ;; Store the original point position for undo restoration
           (add-undo-record buffer :delete-backward-char (copy-list point) deleted-char)
           (delete-backward-char-without-undo buffer)))
        ;; If point is at beginning of line and not at first line, record newline deletion
        ((and (= col 0) (> line-num 0))
         (let ((prev-line (buffer-line buffer (1- line-num))))
           ;; Store the original point position for undo restoration
           (add-undo-record buffer :delete-backward-char (copy-list point) #\Newline)
           (delete-backward-char-without-undo buffer)))
        ;; Otherwise, at beginning of buffer, do nothing
        (t nil)))))

(defun insert-text-without-undo (buffer text)
  "Insert text at point without recording undo information"
  (when (and (> (buffer-line-count buffer) 0) (stringp text) (> (length text) 0))
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (col (second point))
           (current-line (buffer-line buffer line-num))
           (new-line (concatenate 'string 
                                  (subseq current-line 0 col)
                                  text
                                  (subseq current-line col))))
      ;; Update the line with the new text
      (setf (aref (lines buffer) line-num) new-line)
      ;; Move point forward by the length of the inserted text
      (buffer-set-point buffer line-num (+ col (length text))))))

(defun kill-line-without-undo (buffer)
  "Delete from point to end of line without recording undo information"
  (when (> (buffer-line-count buffer) 0)
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (col (second point))
           (current-line (buffer-line buffer line-num)))
      (when (< col (length current-line))
        (let ((new-line (subseq current-line 0 col)))
          (setf (aref (lines buffer) line-num) new-line))))))

(defmethod kill-line ((buffer standard-buffer))
  "Delete from point to end of line"
  (when (> (buffer-line-count buffer) 0)
    ;; Clear the mark before deletion
    (buffer-clear-mark buffer)
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (col (second point))
           (current-line (buffer-line buffer line-num))
           (killed-text (when (< col (length current-line))
                          (subseq current-line col))))
      (when killed-text
        ;; Record undo information with the killed text
        (add-undo-record buffer :kill-line (copy-list point) killed-text)
        ;; Perform the kill operation
        (kill-line-without-undo buffer)))))

(defun insert-whole-line-without-undo (buffer killed-text)
  "Insert a killed line back into the buffer for undo"
  (when (and (> (buffer-line-count buffer) 0) (stringp killed-text))
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (old-lines (lines buffer))
           (old-length (length old-lines))
           (has-newline (and (> (length killed-text) 0) 
                             (char= (char killed-text (1- (length killed-text))) #\Newline)))
           (line-content (if has-newline 
                             (subseq killed-text 0 (1- (length killed-text)))
                             killed-text))
           ;; Ensure insertion point is valid - clamp to valid range
           (safe-line-num (min line-num old-length)))
      (if has-newline
          ;; Killed text had a newline, so insert as new line
          (let ((new-lines (make-array (1+ old-length))))
            ;; Copy lines before insertion point
            (loop for i from 0 below safe-line-num do
              (setf (aref new-lines i) (aref old-lines i)))
            ;; Insert the killed line at safe position
            (setf (aref new-lines safe-line-num) line-content)
            ;; Copy remaining lines after insertion point
            (loop for i from safe-line-num below old-length do
              (setf (aref new-lines (1+ i)) (aref old-lines i)))
            ;; Update buffer
            (setf (lines buffer) new-lines))
          ;; Killed text had no newline, replace current line (if position is valid)
          (if (< line-num old-length)
              (setf (aref old-lines line-num) line-content)
              ;; If position is invalid, append as new line  
              (let ((new-lines (make-array (1+ old-length))))
                (loop for i from 0 below old-length do
                  (setf (aref new-lines i) (aref old-lines i)))
                (setf (aref new-lines old-length) line-content)
                (setf (lines buffer) new-lines)))))))

(defun kill-whole-line-without-undo (buffer)
  "Delete entire current line without recording undo information"
  (when (> (buffer-line-count buffer) 0)
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (old-lines (lines buffer))
           (old-length (length old-lines)))
      (when (> old-length 1)
        ;; More than one line - remove the current line
        (let ((new-lines (make-array (1- old-length))))
          ;; Copy lines before the deleted line
          (loop for i from 0 below line-num do
            (setf (aref new-lines i) (aref old-lines i)))
          ;; Copy lines after the deleted line  
          (loop for i from (1+ line-num) below old-length do
            (setf (aref new-lines (1- i)) (aref old-lines i)))
          ;; Update the buffer with new lines array
          (setf (lines buffer) new-lines)
          ;; Adjust point position based on which line was deleted
          (cond
            ;; If we deleted the last line, move to beginning of previous line (now last line)
            ((>= line-num (length new-lines))
             (buffer-set-point buffer (1- (length new-lines)) 0))
            ;; Otherwise, move to beginning of current line (which is now a different line)
            (t
             (buffer-set-point buffer line-num 0)))))
      (when (= old-length 1)
        ;; Only one line - clear it but keep the line
        (setf (aref old-lines 0) "")
        (buffer-set-point buffer 0 0)))))

(defmethod kill-whole-line ((buffer standard-buffer))
  "Delete entire current line"
  (when (> (buffer-line-count buffer) 0)
    ;; Clear the mark before deletion
    (buffer-clear-mark buffer)
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (current-line (buffer-line buffer line-num))
           ;; Store the entire line including implicit newline (except for last line)
           (killed-text (if (< line-num (1- (buffer-line-count buffer)))
                            (concatenate 'string current-line (string #\Newline))
                            current-line)))
      ;; Record undo information with the killed line
      (add-undo-record buffer :kill-whole-line (copy-list point) killed-text)
      ;; Perform the kill operation
      (kill-whole-line-without-undo buffer))))

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

;; Word boundary functions for kill-word
(defun word-char-p (char)
  "Return true if character is part of a word"
  (or (alphanumericp char)
      (char= char #\_)))

(defun find-word-end (buffer)
  "Find the end position of the current word from point"
  (when (> (buffer-line-count buffer) 0)
    (let* ((point (buffer-get-point buffer))
           (line-num (first point))
           (col (second point)))
      ;; Skip any non-word characters first (within current line)
      (when (< line-num (buffer-line-count buffer))
        (let ((current-line (buffer-line buffer line-num)))
          (loop while (and (< col (length current-line))
                           (not (word-char-p (char current-line col))))
                do (incf col))
          ;; Now find end of word characters (within current line)
          (loop while (and (< col (length current-line))
                           (word-char-p (char current-line col)))
                do (incf col))))
      (list line-num col))))

(defun kill-word-without-undo (buffer)
  "Delete word forward without recording undo information"
  (when (> (buffer-line-count buffer) 0)
    (let* ((start-point (buffer-get-point buffer))
           (start-line (first start-point))
           (start-col (second start-point))
           (end-point (find-word-end buffer))
           (end-line (first end-point))
           (end-col (second end-point)))
      ;; Only delete if we found a valid end point different from start
      (unless (and (= start-line end-line) (= start-col end-col))
        (cond
          ;; Same line deletion
          ((= start-line end-line)
           (let* ((current-line (buffer-line buffer start-line))
                  (new-line (concatenate 'string
                                         (subseq current-line 0 start-col)
                                         (subseq current-line end-col))))
             (setf (aref (lines buffer) start-line) new-line)))
          ;; Multi-line deletion
          (t
           (let* ((start-line-text (buffer-line buffer start-line))
                  (end-line-text (buffer-line buffer end-line))
                  (new-line (concatenate 'string
                                         (subseq start-line-text 0 start-col)
                                         (subseq end-line-text end-col)))
                  (old-lines (lines buffer))
                  (old-length (length old-lines))
                  (lines-to-remove (- end-line start-line))
                  (new-lines (make-array (- old-length lines-to-remove))))
             ;; Copy lines before the deletion
             (loop for i from 0 below start-line do
               (setf (aref new-lines i) (aref old-lines i)))
             ;; Set the joined line
             (setf (aref new-lines start-line) new-line)
             ;; Copy lines after the deletion
             (loop for i from (1+ end-line) below old-length do
               (setf (aref new-lines (+ start-line 1 (- i end-line 1))) (aref old-lines i)))
             ;; Update the buffer
             (setf (lines buffer) new-lines))))))))

(defmethod kill-word ((buffer standard-buffer))
  "Delete word forward"
  (when (> (buffer-line-count buffer) 0)
    ;; Clear the mark before deletion
    (buffer-clear-mark buffer)
    (let* ((start-point (buffer-get-point buffer))
           (start-line (first start-point))
           (start-col (second start-point))
           (end-point (find-word-end buffer))
           (end-line (first end-point))
           (end-col (second end-point)))
      ;; Only proceed if we have something to delete
      (unless (and (= start-line end-line) (= start-col end-col))
        (let ((killed-text
                (cond
                  ;; Same line deletion
                  ((= start-line end-line)
                   (let ((current-line (buffer-line buffer start-line)))
                     (subseq current-line start-col end-col)))
                  ;; Multi-line deletion
                  (t
                   (let ((text ""))
                     ;; Add text from start line
                     (let ((start-line-text (buffer-line buffer start-line)))
                       (setf text (concatenate 'string text (subseq start-line-text start-col))))
                     ;; Add newline if we cross lines
                     (setf text (concatenate 'string text (string #\Newline)))
                     ;; Add complete middle lines
                     (loop for line-num from (1+ start-line) below end-line do
                       (setf text (concatenate 'string text (buffer-line buffer line-num) (string #\Newline))))
                     ;; Add text from end line
                     (when (< end-line (buffer-line-count buffer))
                       (let ((end-line-text (buffer-line buffer end-line)))
                         (setf text (concatenate 'string text (subseq end-line-text 0 end-col)))))
                     text)))))
          ;; Record undo information with the killed text
          (add-undo-record buffer :kill-word (copy-list start-point) killed-text)
          ;; Perform the kill operation
          (kill-word-without-undo buffer))))))
