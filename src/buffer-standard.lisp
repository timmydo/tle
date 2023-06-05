(in-package :tle)

(defclass standard-buffer ()
  ((%lines :initform (make-array 0) :accessor lines)
   (%mark :initform nil :accessor mark)
   (%point :initform nil :accessor point)
   (%history :initform nil :accessor history)
   )
  (:documentation "A stardard buffer implementation"))

(defun make-standard-buffer (name)
  (let ((buf (make-instance 'standard-buffer)))
    (setf (lines buf) #(""))
    (setf (point buf) (cons 0 0))
    (setf (mark buf) nil)
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
