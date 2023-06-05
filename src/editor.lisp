(in-package :tle)

(defclass editor ()
  ()
  (:documentation "An editor implementation"))

(defgeneric editor-windows (editor)
  (:documentation "Get an editor's windows"))

(defgeneric editor-buffers (editor)
  (:documentation "Get an editors buffers"))
