(in-package :tle)

(defclass window ()
  ()
  (:documentation "A window implementation"))

(defgeneric draw-window (ui)
  (:documentation "Draw a window"))

