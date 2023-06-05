(in-package :tle)


(defclass ui-implementation ()
  ((name :initarg :name :reader name))
  (:documentation "A UI implementation"))

(defgeneric run-ui (ui-implementation editor))

