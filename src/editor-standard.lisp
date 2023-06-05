(in-package :tle)

(defclass standard-editor (editor)
  ((%windows :accessor windows)
   (%buffers :accessor buffers)))

(defun make-standard-editor ()
  (let ((e (make-instance 'standard-editor)))
    (setf (buffers e) (list (make-standard-buffer "*scratch*")))
    (setf (windows e) (make-standard-window (first (buffers e))))
    e))

(defmethod editor-windows ((editor standard-editor))
  (windows editor))

(defmethod editor-buffers ((editor standard-editor))
  (buffers editor))

