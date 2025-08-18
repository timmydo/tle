(in-package :tle)

(defclass editor ()
  ()
  (:documentation "An editor implementation"))

(defgeneric editor-buffers (editor)
  (:documentation "Get an editors buffers"))

(defclass standard-editor (editor)
  ((%buffers :accessor buffers)))

(defun make-standard-editor ()
  (let ((e (make-instance 'standard-editor)))
    (setf (buffers e) (list (make-standard-buffer "*scratch*")))
    e))

(defmethod editor-buffers ((editor standard-editor))
  (buffers editor))

(defun current-buffer (editor)
  "Get the current buffer from the editor."
  (first (buffers editor)))

