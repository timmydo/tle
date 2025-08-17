(in-package :tle)

(defclass editor ()
  ()
  (:documentation "An editor implementation"))

(defgeneric editor-windows (editor)
  (:documentation "Get an editor's windows"))

(defgeneric editor-buffers (editor)
  (:documentation "Get an editors buffers"))

(defclass standard-editor (editor)
  ((%windows :accessor windows)
   (%buffers :accessor buffers)))

(defun make-standard-editor ()
  (let ((e (make-instance 'standard-editor)))
    (setf (buffers e) (list (make-standard-buffer "*scratch*")))
    (setf (windows e) (list (make-standard-window (first (buffers e)))))
    e))

(defmethod editor-windows ((editor standard-editor))
  (windows editor))

(defmethod editor-buffers ((editor standard-editor))
  (buffers editor))

(defun current-buffer (editor)
  "Get the current buffer from the editor."
  (first (buffers editor)))

