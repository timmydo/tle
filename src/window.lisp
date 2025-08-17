(in-package :tle)

(defclass window ()
  ((%handle :initform nil :accessor window-handle))
  (:documentation "A window implementation"))

(defgeneric draw-window (window ui)
  (:documentation "Draw a WINDOW using UI"))

(defgeneric window-dispatch-event (window ui-event)
  (:documentation "The WINDOW dispatches a UI-EVENT to the correct view"))



