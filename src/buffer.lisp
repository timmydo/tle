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


(defclass standard-buffer (buffer)
  ((%lines :initform (make-array 0) :accessor lines)
   (%history :initform nil :accessor history)
   (%name :initarg :name :accessor buffer-name :initform "Untitled")
   )
  (:documentation "A stardard buffer implementation"))

(defun make-standard-buffer (name)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #("line 1" "line 2" "line 3"))
    (setf (buffer-name buf) name)
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

(defmethod buffer-mark ((buffer standard-buffer))
  )
