(in-package :tle)

(defclass window ()
  ((%handle :initform nil :accessor window-handle))
  (:documentation "A window implementation"))

(defgeneric draw-window (ui)
  (:documentation "Draw a window"))

